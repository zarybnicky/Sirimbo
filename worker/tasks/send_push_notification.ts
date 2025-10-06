import type { Task } from 'graphile-worker';
import admin from 'firebase-admin';
import webPush, { WebPushError } from 'web-push';

type ChannelRow = {
  id: string;
  provider: 'fcm' | 'web_push';
  platform: 'android' | 'ios' | 'web';
  channelIdentifier: string;
  credentials: Record<string, unknown>;
};

let firebaseApp: admin.app.App | undefined;
let webPushConfigured = false;

function getFirebaseApp(): admin.app.App {
  if (firebaseApp) {
    return firebaseApp;
  }

  const serviceAccountRaw = process.env.FIREBASE_SERVICE_ACCOUNT;
  if (!serviceAccountRaw) {
    throw new Error('FIREBASE_SERVICE_ACCOUNT environment variable must be set for push notifications.');
  }

  let serviceAccount: admin.ServiceAccount;
  try {
    serviceAccount = JSON.parse(serviceAccountRaw);
  } catch (error) {
    throw new Error('FIREBASE_SERVICE_ACCOUNT must contain valid JSON.');
  }

  const existing = admin.apps.find((app) => app.name === 'rozpisovnik-worker');
  if (existing) {
    firebaseApp = existing;
    return firebaseApp;
  }

  firebaseApp = admin.initializeApp({
    credential: admin.credential.cert(serviceAccount),
  }, 'rozpisovnik-worker');

  return firebaseApp;
}

function ensureWebPushConfigured(): void {
  if (webPushConfigured) {
    return;
  }

  const subject = process.env.WEB_PUSH_VAPID_SUBJECT;
  const publicKey = process.env.WEB_PUSH_VAPID_PUBLIC_KEY;
  const privateKey = process.env.WEB_PUSH_VAPID_PRIVATE_KEY;

  if (!subject || !publicKey || !privateKey) {
    throw new Error('WEB_PUSH_VAPID_SUBJECT, WEB_PUSH_VAPID_PUBLIC_KEY, and WEB_PUSH_VAPID_PRIVATE_KEY must be set.');
  }

  webPush.setVapidDetails(subject, publicKey, privateKey);
  webPushConfigured = true;
}

function normaliseData(data?: Record<string, unknown>): Record<string, string> | undefined {
  if (!data) {
    return undefined;
  }

  const entries: Array<[string, string]> = [];
  for (const [key, value] of Object.entries(data)) {
    if (value === undefined || value === null) {
      continue;
    }
    if (typeof value === 'string') {
      entries.push([key, value]);
      continue;
    }
    if (typeof value === 'number' || typeof value === 'boolean') {
      entries.push([key, String(value)]);
      continue;
    }
    entries.push([key, JSON.stringify(value)]);
  }

  return entries.length > 0 ? Object.fromEntries(entries) : undefined;
}

const task: Task<'send_push_notification'> = async (payload, workerUtils) => {
  const { userId, notification, data, android, ios, web } = payload;

  if (!notification?.title || !notification?.body) {
    throw new Error('Push notification payload must include both a title and body.');
  }

  const { rows: channels } = await workerUtils.withPgClient((pgClient) =>
    pgClient.query<ChannelRow>(
      `select
        id::text as id,
        provider,
        platform::text as platform,
        channel_identifier as "channelIdentifier",
        credentials
      from public.push_notification_channel
      where user_id = $1
        and is_active is true`,
      [userId],
    ),
  );

  if (!channels.length) {
    console.warn(`No push notification channels registered for user ${userId}.`);
    return;
  }

  const fcmChannels = channels.filter((channel) => channel.provider === 'fcm');
  if (fcmChannels.length) {
    const messaging = getFirebaseApp().messaging();
    const tokens = Array.from(new Set(fcmChannels.map((channel) => channel.channelIdentifier)));

    const message: admin.messaging.MulticastMessage = {
      tokens,
      notification: {
        title: notification.title,
        body: notification.body,
      },
      data: normaliseData(data),
    };

    if (notification.image) {
      message.notification = {
        ...message.notification,
        imageUrl: notification.image,
      };
    }

    const androidConfig: admin.messaging.AndroidConfig = {
      priority: android?.priority ?? 'high',
      notification: {},
    };

    if (notification.icon) {
      androidConfig.notification = {
        ...androidConfig.notification,
        icon: notification.icon,
      };
    }
    if (android?.color) {
      androidConfig.notification = {
        ...androidConfig.notification,
        color: android.color,
      };
    }
    if (notification.clickAction) {
      androidConfig.notification = {
        ...androidConfig.notification,
        clickAction: notification.clickAction,
      };
    }
    if (android?.channelId) {
      androidConfig.notification = {
        ...androidConfig.notification,
        channelId: android.channelId,
      };
    }
    if (notification.image) {
      androidConfig.notification = {
        ...androidConfig.notification,
        imageUrl: notification.image,
      };
    }
    if (android?.ttlSeconds !== undefined) {
      androidConfig.ttl = `${android.ttlSeconds}s`;
    }

    if (Object.keys(androidConfig.notification ?? {}).length > 0 || androidConfig.ttl || androidConfig.priority) {
      message.android = androidConfig;
    }

    const apnsAlert: admin.messaging.ApnsAlert = {
      title: notification.title,
      body: notification.body,
    };

    const aps: admin.messaging.Aps = {
      alert: apnsAlert,
    };

    if (ios?.sound) {
      aps.sound = ios.sound;
    }
    if (ios?.badge !== undefined) {
      aps.badge = ios.badge;
    }
    if (ios?.category) {
      aps.category = ios.category;
    }
    if (ios?.threadId) {
      aps.threadId = ios.threadId;
    }

    message.apns = {
      payload: {
        aps,
      },
    };

    try {
      const response = await messaging.sendEachForMulticast(message);
      const invalidTokens: string[] = [];
      const successfulTokens: string[] = [];
      const retriableErrors: Array<{ token: string; code?: string; message?: string }> = [];

      response.responses.forEach((res, index) => {
        const token = tokens[index];
        if (res.success) {
          successfulTokens.push(token);
          return;
        }
        const errorCode = res.error?.code ?? '';
        if (
          errorCode === 'messaging/registration-token-not-registered' ||
          errorCode === 'messaging/invalid-registration-token'
        ) {
          invalidTokens.push(token);
        } else {
          console.error('Failed to send FCM push', {
            token,
            error: res.error?.message,
            code: res.error?.code,
          });
          retriableErrors.push({
            token,
            code: res.error?.code,
            message: res.error?.message,
          });
        }
      });

      if (successfulTokens.length) {
        await workerUtils.withPgClient((pgClient) =>
          pgClient.query(
            `update public.push_notification_channel
             set last_notified_at = now()
             where provider = 'fcm'
               and channel_identifier = any($1::text[])`,
            [successfulTokens],
          ),
        );
      }

      if (invalidTokens.length) {
        await workerUtils.withPgClient((pgClient) =>
          pgClient.query(
            `update public.push_notification_channel
             set is_active = false
             where provider = 'fcm'
               and channel_identifier = any($1::text[])`,
            [invalidTokens],
          ),
        );
      }

      if (retriableErrors.length) {
        console.error('Retrying push notification for FCM tokens due to transient failures', {
          errors: retriableErrors,
        });
        const error = new Error('Transient FCM failure; will retry notification.');
        (error as Error & { retriableErrors?: typeof retriableErrors }).retriableErrors = retriableErrors;
        throw error;
      }
    } catch (error) {
      if (error && typeof error === 'object' && 'retriableErrors' in error) {
        throw error;
      }
      console.error('Unexpected error while sending FCM notification', error);
      throw error;
    }
  }

  const webPushChannels = channels.filter((channel) => channel.provider === 'web_push');
  if (webPushChannels.length) {
    ensureWebPushConfigured();

    const payloadData: Record<string, unknown> = {
      ...data,
    };
    if (notification.clickAction) {
      payloadData.clickAction = notification.clickAction;
    }

    const notificationPayload = {
      title: notification.title,
      body: notification.body,
      icon: web?.icon ?? notification.icon,
      image: web?.image ?? notification.image,
      badge: web?.badge,
      actions: web?.actions,
      requireInteraction: web?.requireInteraction,
      data: payloadData,
    };

    const options = web?.ttlSeconds !== undefined ? { TTL: web.ttlSeconds } : undefined;

    const successfulEndpoints: string[] = [];
    const invalidEndpoints: string[] = [];
    const retriableErrors: Array<{ endpoint: string; statusCode?: number; body?: string; message?: string }> = [];

    for (const channel of webPushChannels) {
      const { channelIdentifier, credentials } = channel;
      const p256dh = typeof credentials?.p256dh === 'string' ? credentials.p256dh : undefined;
      const auth = typeof credentials?.auth === 'string' ? credentials.auth : undefined;

      if (!p256dh || !auth) {
        console.warn(`Missing Web Push keys for channel ${channelIdentifier}; disabling.`);
        invalidEndpoints.push(channelIdentifier);
        continue;
      }

      const subscription = {
        endpoint: channelIdentifier,
        keys: {
          p256dh,
          auth,
        },
      };

      try {
        await webPush.sendNotification(subscription, JSON.stringify(notificationPayload), options);
        successfulEndpoints.push(channelIdentifier);
      } catch (error) {
        if (error instanceof WebPushError && (error.statusCode === 404 || error.statusCode === 410)) {
          invalidEndpoints.push(channelIdentifier);
        } else {
          console.error('Failed to deliver Web Push notification', {
            endpoint: channelIdentifier,
            error,
          });
          if (error instanceof WebPushError) {
            retriableErrors.push({
              endpoint: channelIdentifier,
              statusCode: error.statusCode,
              body: error.body,
              message: error.message,
            });
          } else {
            retriableErrors.push({
              endpoint: channelIdentifier,
              message: error instanceof Error ? error.message : undefined,
            });
          }
        }
      }
    }

    if (successfulEndpoints.length) {
      await workerUtils.withPgClient((pgClient) =>
        pgClient.query(
          `update public.push_notification_channel
           set last_notified_at = now()
           where provider = 'web_push'
             and channel_identifier = any($1::text[])`,
          [successfulEndpoints],
        ),
      );
    }

    if (invalidEndpoints.length) {
      await workerUtils.withPgClient((pgClient) =>
        pgClient.query(
          `update public.push_notification_channel
           set is_active = false
           where provider = 'web_push'
             and channel_identifier = any($1::text[])`,
          [invalidEndpoints],
        ),
      );
    }

    if (retriableErrors.length) {
      console.error('Retrying push notification for Web Push endpoints due to transient failures', {
        errors: retriableErrors,
      });
      const error = new Error('Transient Web Push failure; will retry notification.');
      (error as Error & { retriableErrors?: typeof retriableErrors }).retriableErrors = retriableErrors;
      throw error;
    }
  }
};

export default task;

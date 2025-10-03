import { fetchGql } from '@/graphql/query';
import { useAuth } from '@/ui/use-auth';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import {
  DeactivateWebPushChannelDocument,
  type DeactivateWebPushChannelMutation,
  type DeactivateWebPushChannelMutationVariables,
  UpsertWebPushChannelDocument,
  type UpsertWebPushChannelMutation,
  type UpsertWebPushChannelMutationVariables,
  WebPushChannelsDocument,
  type WebPushChannel,
  type WebPushChannelsQuery,
  type WebPushChannelsQueryVariables,
} from './graphql';

type RegistrationStatus =
  | 'checking'
  | 'missing-config'
  | 'unsupported'
  | 'permission-blocked'
  | 'requires-auth'
  | 'ready'
  | 'registering'
  | 'registered'
  | 'error';

interface RegistrationState {
  status: RegistrationStatus;
  error?: string;
  channels: WebPushChannel[];
  subscription?: PushSubscriptionJSON | null;
  activeChannelId?: string | null;
}

const PUSH_SERVICE_WORKER_URL = '/push-service-worker.js';

const base64ToUint8Array = (value: string) => {
  const padding = '='.repeat((4 - (value.length % 4)) % 4);
  const base64 = (value + padding).replace(/-/g, '+').replace(/_/g, '/');
  const rawData = globalThis.atob(base64);
  const outputArray = new Uint8Array(rawData.length);

  for (let i = 0; i < rawData.length; i += 1) {
    outputArray[i] = rawData.charCodeAt(i);
  }

  return outputArray;
};

const mergeChannels = (
  channels: WebPushChannel[],
  channel: WebPushChannel,
): WebPushChannel[] => {
  const next = [...channels];
  const index = next.findIndex((item) => item.id === channel.id);
  if (index >= 0) {
    next[index] = channel;
  } else {
    next.unshift(channel);
  }
  return next;
};

export const useWebPushRegistration = () => {
  const auth = useAuth();
  const vapidKey = process.env.NEXT_PUBLIC_WEB_PUSH_VAPID_PUBLIC_KEY;
  const userId = auth.user?.id ?? null;

  const [state, setState] = useState<RegistrationState>({
    status: 'checking',
    channels: [],
  });
  const pendingSubscription = useRef<PushSubscription | null>(null);

  const isBrowser = typeof window !== 'undefined';

  const requireUserId = useCallback((): string | null => {
    if (!userId) {
      setState((prev) => ({
        ...prev,
        status: 'requires-auth',
        error: undefined,
        channels: [],
        subscription: null,
        activeChannelId: null,
      }));
      return null;
    }
    return userId;
  }, [userId]);

  const fetchChannels = useCallback(async (): Promise<WebPushChannel[]> => {
    const id = requireUserId();
    if (!id) return [];
    const result = await fetchGql<WebPushChannelsQuery, WebPushChannelsQueryVariables>(
      WebPushChannelsDocument,
      { userId: id },
    );
    return result.pushNotificationChannelsList ?? [];
  }, [requireUserId]);

  const persistSubscription = useCallback(
    async (subscription: PushSubscription) => {
      const id = requireUserId();
      if (!id) return null;

      const payload = subscription.toJSON();
      if (!payload.endpoint) {
        throw new Error('Subscription endpoint missing.');
      }
      const credentials: Record<string, unknown> = {
        ...((payload.keys as Record<string, unknown> | undefined) ?? {}),
        expirationTime: payload.expirationTime,
        userAgent: isBrowser ? navigator.userAgent : undefined,
      };

      const result = await fetchGql<
        UpsertWebPushChannelMutation,
        UpsertWebPushChannelMutationVariables
      >(UpsertWebPushChannelDocument, {
        input: {
          pushNotificationChannel: {
            userId: id,
            platform: 'WEB',
            provider: 'web_push',
            channelIdentifier: payload.endpoint,
            credentials,
            isActive: true,
            lastRegisteredAt: new Date().toISOString(),
          },
          onConflict: {
            constraint: 'pushNotificationChannelProviderIdentifierKey',
            updateColumns: [
              'CREDENTIALS',
              'IS_ACTIVE',
              'LAST_REGISTERED_AT',
              'PLATFORM',
              'USER_ID',
            ],
          },
        },
      });

      return result.createPushNotificationChannel?.pushNotificationChannel ?? null;
    },
    [requireUserId, isBrowser],
  );

  const deactivateChannel = useCallback(
    async (channelId: string) => {
      await fetchGql<
        DeactivateWebPushChannelMutation,
        DeactivateWebPushChannelMutationVariables
      >(DeactivateWebPushChannelDocument, {
        input: { id: channelId, patch: { isActive: false } },
      });
    },
    [],
  );

  const refresh = useCallback(async () => {
    if (!isBrowser) return;
    const id = requireUserId();
    if (!id) return;

    if (!vapidKey) {
      setState({
        status: 'missing-config',
        error: undefined,
        channels: [],
        subscription: null,
        activeChannelId: null,
      });
      return;
    }

    if (
      !('serviceWorker' in navigator) ||
      !('PushManager' in window) ||
      !('Notification' in window)
    ) {
      setState({
        status: 'unsupported',
        error: undefined,
        channels: [],
        subscription: null,
        activeChannelId: null,
      });
      return;
    }

    if (Notification.permission === 'denied') {
      setState({
        status: 'permission-blocked',
        error: undefined,
        channels: [],
        subscription: null,
        activeChannelId: null,
      });
      return;
    }

    setState((prev) => ({ ...prev, status: 'checking', error: undefined }));

    let channels: WebPushChannel[] = [];
    try {
      channels = await fetchChannels();
    } catch (error) {
      console.error('Failed to load push channels', error);
    }

    const registration = await navigator.serviceWorker.getRegistration(
      PUSH_SERVICE_WORKER_URL,
    );

    if (!registration) {
      setState({
        status: 'ready',
        error: undefined,
        channels,
        subscription: null,
        activeChannelId: null,
      });
      return;
    }

    const subscription = await registration.pushManager.getSubscription();

    if (!subscription) {
      setState({
        status: 'ready',
        error: undefined,
        channels,
        subscription: null,
        activeChannelId: null,
      });
      return;
    }

    try {
      const channel = await persistSubscription(subscription);
      const mergedChannels = channel
        ? mergeChannels(channels, channel)
        : channels;

      setState({
        status: 'registered',
        error: undefined,
        channels: mergedChannels,
        subscription: subscription.toJSON(),
        activeChannelId: channel?.id ?? null,
      });
    } catch (error) {
      console.error('Failed to persist subscription', error);
      setState({
        status: 'error',
        error: error instanceof Error ? error.message : String(error),
        channels,
        subscription: subscription.toJSON(),
        activeChannelId: null,
      });
    }
  }, [fetchChannels, isBrowser, persistSubscription, requireUserId, vapidKey]);

  const register = useCallback(async () => {
    if (!isBrowser) return;
    if (!requireUserId()) return;
    if (!vapidKey) {
      setState((prev) => ({
        ...prev,
        status: 'missing-config',
        error: 'Chybí veřejný VAPID klíč pro web push notifikace.',
      }));
      return;
    }

    if (
      !('serviceWorker' in navigator) ||
      !('PushManager' in window) ||
      !('Notification' in window)
    ) {
      setState((prev) => ({
        ...prev,
        status: 'unsupported',
        error: 'Tento prohlížeč nepodporuje web push notifikace.',
      }));
      return;
    }

    if (Notification.permission === 'denied') {
      setState((prev) => ({
        ...prev,
        status: 'permission-blocked',
        error: 'Povolení pro notifikace je zablokováno.',
      }));
      return;
    }

    const permission = Notification.permission;
    const finalPermission =
      permission === 'default'
        ? await Notification.requestPermission()
        : permission;

    if (finalPermission !== 'granted') {
      setState((prev) => ({
        ...prev,
        status: 'permission-blocked',
        error: 'Bez povolení nelze notifikace aktivovat.',
      }));
      return;
    }

    setState((prev) => ({ ...prev, status: 'registering', error: undefined }));

    try {
      const registration = await navigator.serviceWorker.register(
        PUSH_SERVICE_WORKER_URL,
        { scope: '/' },
      );
      const readyRegistration = await navigator.serviceWorker.ready;
      const targetRegistration = readyRegistration || registration;

      pendingSubscription.current = await targetRegistration.pushManager.subscribe({
        userVisibleOnly: true,
        applicationServerKey: base64ToUint8Array(vapidKey),
      });

      const channel = await persistSubscription(pendingSubscription.current);
      const nextChannels = channel
        ? mergeChannels(state.channels, channel)
        : state.channels;

      setState({
        status: 'registered',
        error: undefined,
        channels: nextChannels,
        subscription: pendingSubscription.current.toJSON(),
        activeChannelId: channel?.id ?? null,
      });
    } catch (error) {
      console.error('Failed to register push subscription', error);
      setState({
        status: 'error',
        error: error instanceof Error ? error.message : String(error),
        channels: state.channels,
        subscription: null,
        activeChannelId: null,
      });
    } finally {
      pendingSubscription.current = null;
    }
  }, [isBrowser, persistSubscription, requireUserId, state.channels, vapidKey]);

  const unregister = useCallback(async () => {
    if (!isBrowser) return;

    setState((prev) => ({ ...prev, status: 'checking' }));

    const registration = await navigator.serviceWorker.getRegistration(
      PUSH_SERVICE_WORKER_URL,
    );
    const subscription = await registration?.pushManager.getSubscription();

    if (subscription) {
      try {
        if (state.activeChannelId) {
          await deactivateChannel(state.activeChannelId);
        }
      } catch (error) {
        console.error('Failed to deactivate push channel', error);
      }

      try {
        await subscription.unsubscribe();
      } catch (error) {
        console.error('Failed to unsubscribe from push manager', error);
      }
    }

    setState((prev) => ({
      ...prev,
      status: 'ready',
      channels: prev.channels.filter((channel) => channel.id !== prev.activeChannelId),
      subscription: null,
      activeChannelId: null,
    }));
  }, [deactivateChannel, isBrowser, state.activeChannelId]);

  const removeChannel = useCallback(
    async (channelId: string) => {
      try {
        await deactivateChannel(channelId);
        setState((prev) => ({
          ...prev,
          channels: prev.channels.filter((channel) => channel.id !== channelId),
          ...(prev.activeChannelId === channelId
            ? { activeChannelId: null, status: 'ready', subscription: null }
            : {}),
        }));
      } catch (error) {
        console.error('Failed to remove channel', error);
        setState((prev) => ({
          ...prev,
          status: 'error',
          error: error instanceof Error ? error.message : String(error),
        }));
      }
    },
    [deactivateChannel],
  );

  useEffect(() => {
    if (!isBrowser) return;
    refresh().catch((error) => {
      console.error('Failed to bootstrap push registration state', error);
      setState((prev) => ({
        ...prev,
        status: 'error',
        error: error instanceof Error ? error.message : String(error),
      }));
    });
  }, [isBrowser, refresh]);

  useEffect(() => {
    if (!isBrowser) return;
    const listener = (event: MessageEvent) => {
      if (event.data?.type === 'push-subscription-changed') {
        refresh().catch((error) => {
          console.error('Failed to refresh push registration after subscription change', error);
        });
      }
    };
    navigator.serviceWorker.addEventListener('message', listener);
    return () => {
      navigator.serviceWorker.removeEventListener('message', listener);
    };
  }, [isBrowser, refresh]);

  const isSupported = useMemo(
    () =>
      isBrowser &&
      'serviceWorker' in navigator &&
      'PushManager' in window &&
      'Notification' in window,
    [isBrowser],
  );

  return {
    status: state.status,
    error: state.error,
    isSupported,
    channels: state.channels,
    subscription: state.subscription ?? null,
    activeChannelId: state.activeChannelId ?? null,
    register,
    unregister,
    removeChannel,
    refresh,
  };
};

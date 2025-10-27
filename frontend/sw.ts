import { defaultCache } from "@serwist/next/worker";
import { Serwist, type PrecacheEntry } from 'serwist';

declare const self: ServiceWorkerGlobalScope & {
  __SW_MANIFEST: Array<PrecacheEntry | string> | undefined;
};

const serwist = new Serwist({
  precacheEntries: self.__SW_MANIFEST!,
  skipWaiting: false,
  clientsClaim: true,
  runtimeCaching: defaultCache,
  precacheOptions: {
    cleanupOutdatedCaches: true,
  },
});
serwist.addEventListeners();

type PushPayload = {
  title?: string;
  body?: string;
  icon?: string;
  badge?: string;
  url?: string;
  data?: Record<string, unknown>;
  tag?: string;
  requireInteraction?: boolean;
};

self.addEventListener('push', event => {
  let payload: PushPayload = {};
  try {
    payload = event.data?.json() as PushPayload || {};
  } catch (error) {
    console.warn('service-worker: failed to parse push payload as JSON', error);
    payload = { body: event.data?.text() };
  }
  const title = payload.title ?? 'Rozpisovník';
  const notificationOptions: NotificationOptions = {
    body: payload.body,
    icon: payload.icon ?? '/olymp/android-chrome-192x192.png',
    badge: payload.badge ?? payload.icon,
    data: { ...payload.data, url: payload.url },
    tag: payload.tag,
    requireInteraction: payload.requireInteraction,
  };

  event.waitUntil(self.registration.showNotification(title, notificationOptions));
});

self.addEventListener('notificationclick', event => {
  event.notification.close();
  const targetUrl = (event.notification.data as { url?: string } | undefined)?.url;

  event.waitUntil(
    (async () => {
      const allClients = await self.clients.matchAll({ type: 'window', includeUncontrolled: true });
      for (const client of allClients) {
        if ('focus' in client) {
          if (targetUrl && 'navigate' in client && client.url !== targetUrl) {
            await client.navigate(targetUrl);
          }
          await client.focus();
          return;
        }
      }
      if (targetUrl) {
        await self.clients.openWindow(targetUrl);
      }
    })(),
  );
});

self.addEventListener('message', event => {
  if ((event.data as { type?: string } | undefined)?.type === 'SKIP_WAITING') {
    void self.skipWaiting();
  }
});

export {};

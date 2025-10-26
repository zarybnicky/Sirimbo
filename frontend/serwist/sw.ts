/// <reference lib="webworker" />

import { ExpirationPlugin, NetworkFirst, StaleWhileRevalidate, type PrecacheEntry, type RuntimeCaching } from 'serwist';
import { installSerwist } from 'serwist/legacy';
import { defaultCache } from "@serwist/next/worker";

declare const self: ServiceWorkerGlobalScope & {
  __SW_MANIFEST: Array<PrecacheEntry | string> | undefined;
};

const precacheEntries = self.__SW_MANIFEST ?? [];

installSerwist({
  precacheEntries: self.__SW_MANIFEST!,
  cleanupOutdatedCaches: true,
  skipWaiting: false,
  runtimeCaching: defaultCache,
});

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

function parsePushEvent(event: PushEvent): PushPayload {
  if (!event.data) return {};
  try {
    return event.data.json() as PushPayload;
  } catch (error) {
    console.warn('service-worker: failed to parse push payload as JSON', error);
    return { body: event.data.text() };
  }
}

self.addEventListener('push', event => {
  const payload = parsePushEvent(event);
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
  if (!event.data || (event.data as { type?: string } | undefined)?.type === 'SKIP_WAITING') {
    void self.skipWaiting();
  }
});

export {};

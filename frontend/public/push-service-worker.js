self.addEventListener('install', (event) => {
  self.skipWaiting();
});

self.addEventListener('activate', (event) => {
  event.waitUntil(self.clients.claim());
});

const parsePayload = (event) => {
  if (!event.data) return {};
  try {
    return event.data.json();
  } catch (error) {
    console.error('Failed to parse push payload', error);
    try {
      const text = event.data.text();
      return { body: text };
    } catch {
      return {};
    }
  }
};

self.addEventListener('push', (event) => {
  const payload = parsePayload(event);
  const title = payload.title || 'Rozpisovník';
  const options = {
    body: payload.body || undefined,
    data: payload.data || {},
    icon: payload.icon || undefined,
    badge: payload.badge || payload.icon || undefined,
    image: payload.image || undefined,
    actions: payload.actions || undefined,
    tag: payload.tag || undefined,
    renotify: payload.renotify || false,
    requireInteraction: payload.requireInteraction || false,
  };

  event.waitUntil(self.registration.showNotification(title, options));
});

const payloadFallbackUrl = (notification) => {
  try {
    const raw = notification?.data;
    if (raw && typeof raw.fallbackUrl === 'string') {
      return raw.fallbackUrl;
    }
  } catch {
    // ignore
  }
  return null;
};

self.addEventListener('notificationclick', (event) => {
  event.notification.close();
  const targetUrl =
    (event.notification.data &&
      (event.notification.data.url || event.notification.data.link)) ||
    payloadFallbackUrl(event.notification);

  event.waitUntil(
    (async () => {
      if (targetUrl) {
        await self.clients.openWindow(targetUrl);
        return;
      }
      const clientList = await self.clients.matchAll({
        type: 'window',
        includeUncontrolled: true,
      });
      if (clientList.length > 0) {
        clientList[0].focus();
      } else {
        await self.clients.openWindow('/');
      }
    })(),
  );
});

self.addEventListener('pushsubscriptionchange', () => {
  self.clients
    .matchAll({ type: 'window', includeUncontrolled: true })
    .then((clientList) => {
      clientList.forEach((client) => {
        client.postMessage({ type: 'push-subscription-changed' });
      });
    });
});

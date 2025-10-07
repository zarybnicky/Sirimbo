import React from 'react';
import { useRouter } from 'next/router';
import { GoogleAnalytics } from 'nextjs-google-analytics';
import type { init, pageView } from 'react-facebook-pixel';
import { tenantConfig } from '@/tenant/config.js';

export const Tracking = React.memo(function Tracking() {
  const router = useRouter();
  const lastTrackedPath = React.useRef<string>();
  const facebookPixelId = tenantConfig.facebookPixelId;
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof pageView;
  } | null>(null);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (!facebookPixelId) return;
    let disposed = false;
    (async () => {
      const facebook = (await import('react-facebook-pixel').then(x => x.default));
      if (disposed) return;
      facebook.init(facebookPixelId);
      facebook.pageView();
      facebookRef.current = facebook;
    })();
    return () => {
      disposed = true;
      facebookRef.current = null;
    };
  }, [facebookPixelId]);

  const track = React.useCallback((path: string) => {
    if (process.env.NODE_ENV === 'development') return;
    if (!facebookPixelId) return;
    if (path === lastTrackedPath.current) return;
    lastTrackedPath.current = path;
    facebookRef.current?.pageView();
  }, [facebookPixelId]);

  React.useEffect(() => {
    router.events.on('routeChangeComplete', track);
    return () => router.events.off('routeChangeComplete', track);
  }, [track, router.events]);

  return (
    <GoogleAnalytics trackPageViews={{ ignoreHashChange: true }} />
  );
});

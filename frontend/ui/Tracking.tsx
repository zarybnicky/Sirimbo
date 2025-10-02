import React from 'react';
import type { init, pageView } from 'react-facebook-pixel';
import { useRouter } from 'next/router';
import { GoogleAnalytics } from 'nextjs-google-analytics';

export const Tracking = React.memo(function Tracking() {
  const router = useRouter();
  const lastTrackedPath = React.useRef<string>();
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof pageView;
  } | null>(null);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    (async () => {
      const facebook = (await import('react-facebook-pixel').then(x => x.default));
      facebook.init('704526480597551');
      facebook.pageView();
      facebookRef.current = facebook;
    })();
  }, []);

  const track = React.useCallback((path: string) => {
    if (process.env.NODE_ENV === 'development') return;
    if (path === lastTrackedPath.current) return;
    lastTrackedPath.current = path;
    facebookRef.current?.pageView();
  }, []);

  React.useEffect(() => {
    router.events.on('routeChangeComplete', track);
    return () => router.events.off('routeChangeComplete', track);
  }, [track, router.events]);

  return (
    <GoogleAnalytics trackPageViews={{ ignoreHashChange: true }} />
  );
});

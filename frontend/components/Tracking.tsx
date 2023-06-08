import React from 'react';
import type { PostHog } from 'posthog-js';
import type { init, pageView } from 'react-facebook-pixel';
import { useRouter } from 'next/router';
import { GoogleAnalytics } from 'nextjs-google-analytics';
import { useAuth } from 'lib/data/use-auth';

export const Tracking = React.memo(function Tracking() {
  const router = useRouter();
  const { user } = useAuth();
  const lastTrackedPath = React.useRef<string>();
  const posthogRef = React.useRef<PostHog | null>(null);
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof pageView;
  } | null>(null);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    (async () => {
      const posthog = (await import('posthog-js')).posthog;
      posthog.init('phc_H2WM9q2xXVFl1wEak9TcQVAsOpWNGuauzAvyOmBquYQ', {
        api_host: `${window.origin}/ingest`,
      });
      posthogRef.current = posthog;
    })();
  }, []);

  React.useEffect(() => {
    if (user) {
      posthogRef.current?.identify(user.uLogin, user);
    }
  }, [user, posthogRef.current]);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    (async () => {
      const facebook = (await import('react-facebook-pixel')).default;
      facebook.init('704526480597551');
      facebook.pageView();
      facebookRef.current = facebook;
    })();
  }, []);

  const track = React.useCallback((path: string) => {
    if (process.env.NODE_ENV === 'development') return;
    if (path === lastTrackedPath.current) return;
    lastTrackedPath.current = path;
    posthogRef.current?.capture('$pageview');
    facebookRef.current?.pageView();
  }, []);

  React.useEffect(() => {
    router.events.on('routeChangeComplete', track);
    return () => router.events.off('routeChangeComplete', track);
  }, [track, router.events]);

  return (
    <GoogleAnalytics
      gaMeasurementId="UA-44456908-1"
      trackPageViews={{ ignoreHashChange: true }}
    />
  );
});

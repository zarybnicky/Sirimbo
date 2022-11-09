import React from "react";
import posthog from "posthog-js";
import { useRouter } from "next/router";

export const Tracking = React.memo(function Tracking() {
  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;

    posthog.init('phc_H2WM9q2xXVFl1wEak9TcQVAsOpWNGuauzAvyOmBquYQ', {
      api_host: 'https://eu.posthog.com',
    });
  }, [])

  const router = useRouter();
  const lastTrackedPath = React.useRef();
  const track = React.useCallback(path => {
    if (process.env.NODE_ENV === 'development') return;
    if (path === lastTrackedPath.current) return;
    lastTrackedPath.current = path;
    posthog.capture('$pageview');
  }, []);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (router.isReady) {
      track(router.asPath)
    }
  }, [router.isReady])

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    router.events.on('routeChangeComplete', track)
    return () => router.events.off('routeChangeComplete', track);
  }, [track]);

  return null;
});

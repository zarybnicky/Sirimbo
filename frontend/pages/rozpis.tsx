import React from 'react';
import { Calendar } from '@/calendar/Calendar';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { useAtomValue } from 'jotai';
import { isDraggingAtom } from '@/calendar/state.ts';
import { cn } from '@/lib/cn.ts';

export default function CalendarPage() {
  useLayoutEffect(() => {
    if (typeof window !== 'undefined' && navigator.userAgent.includes('Szn')) {
      window.document.body.style.overscrollBehaviorY = 'contain';
    }
  }, []);

  const isDragging = useAtomValue(isDraggingAtom);
  return (
    <Layout
      requireMember
      className={cn(
        'grow overflow-hidden overscroll-contain relative h-[calc(100dvh-68px)] lg:h-[calc(100dvh)]',
        isDragging ? 'rbc-is-dragging' : '',
      )}
    >
      <NextSeo title="Rozpis" />

      <div className="overscroll-contain h-[calc(100dvh-68px)] lg:h-full rbc-calendar col-full min-w-0 max-w-full overflow-hidden">
        <Calendar scrollView />
      </div>
    </Layout>
  );
}

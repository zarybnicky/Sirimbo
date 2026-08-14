'use client';

import React from 'react';
import { Calendar } from '@/calendar/Calendar';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { useAtomValue } from 'jotai';
import { isDraggingAtom } from '@/calendar/state.ts';
import { cn } from '@/lib/cn.ts';

export function Schedule() {
  useLayoutEffect(() => {
    if (typeof window !== 'undefined' && navigator.userAgent.includes('Szn')) {
      window.document.body.style.overscrollBehaviorY = 'contain';
    }
  }, []);

  const isDragging = useAtomValue(isDraggingAtom);
  return (
    <div
      className={cn(
        'overscroll-contain h-[calc(100dvh-68px)] lg:h-full rbc-calendar col-full min-w-0 max-w-full overflow-hidden',
        isDragging ? 'rbc-is-dragging' : '',
      )}
    >
      <Calendar scrollView />
    </div>
  );
}

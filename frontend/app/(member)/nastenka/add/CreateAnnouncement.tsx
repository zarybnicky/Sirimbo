'use client';

import React from 'react';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { useRouter } from 'next/navigation';

export function CreateAnnouncement() {
  const router = useRouter();
  const onSuccess = React.useCallback(
    (id: string | undefined) => {
      if (!id) return;
      router.push(`/nastenka/${id}`);
    },
    [router],
  );
  return <AnnouncementForm onSuccess={onSuccess} />;
}

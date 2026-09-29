'use client';

import React from 'react';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { FormResultContext } from '@/ui/form';
import { useRouter } from 'next/navigation';

export function CreateAnnouncement() {
  const router = useRouter();
  const onSuccess = React.useCallback(
    (id: string | undefined) => {
      if (!id) return;
      router.replace(`/nastenka/${id}`);
    },
    [router],
  );
  return (
    <FormResultContext.Provider value={{ onSuccess }}>
      <AnnouncementForm />
    </FormResultContext.Provider>
  );
}

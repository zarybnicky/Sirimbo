'use client';

import { useRouter } from 'next/navigation';
import { Dialog, DialogContent } from '@/ui/dialog';
import { Announcement } from '@/app/(member)/nastenka/[id]/Announcement';

export function AnnouncementDialog({ id }: { id: string }) {
  const router = useRouter();

  return (
    <Dialog open onOpenChange={(open) => !open && router.back()}>
      <DialogContent className="sm:max-w-3xl">
        <Announcement id={id} />
      </DialogContent>
    </Dialog>
  );
}

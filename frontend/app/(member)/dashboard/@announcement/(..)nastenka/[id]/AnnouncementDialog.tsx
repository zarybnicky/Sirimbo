'use client';

import { useRouter } from 'next/navigation';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import { Announcement } from '@/app/(member)/nastenka/[id]/Announcement';

export function AnnouncementDialog({ id }: { id: string }) {
  const router = useRouter();

  return (
    <Dialog open onOpenChange={(open) => !open && router.back()}>
      <DialogContent className="sm:max-w-3xl">
        <DialogTitle className="sr-only">Příspěvek</DialogTitle>
        <Announcement id={id} />
      </DialogContent>
    </Dialog>
  );
}

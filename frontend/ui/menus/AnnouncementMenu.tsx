import {
  type AnnouncementFragment,
  DeleteAnnouncementDocument,
  ToggleUpozorneniStickyDocument,
  ToggleUpozorneniVisibleDocument,
} from '@/graphql/Announcement';
import { useConfirm } from '@/ui/Confirm';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { useAuth } from '@/ui/use-auth';
import { useRouter } from 'next/router';
import React from 'react';
import { useMutation } from 'urql';
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';

export function AnnouncementMenu({
  children,
  item,
  onEdit,
  ...props
}: { item: AnnouncementFragment; onEdit: () => void } & DropdownMenuContentProps) {
  const auth = useAuth();
  const router = useRouter();
  const confirm = useConfirm();

  const doHide = useMutation(ToggleUpozorneniVisibleDocument)[1];
  const doSticky = useMutation(ToggleUpozorneniStickyDocument)[1];
  const doDelete = useMutation(DeleteAnnouncementDocument)[1];

  const sticky = React.useCallback(
    () => doSticky({ id: item.id, sticky: !item.sticky }),
    [item, doSticky],
  );
  const hide = React.useCallback(
    () => doHide({ id: item.id, visible: !item.isVisible }),
    [item, doHide],
  );

  const remove = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat příspěvek "${item.upNadpis}"?` });
    await doDelete({ id: item.id });
    router.replace('/nastenka');
  }, [confirm, doDelete, router, item]);

  if (!auth.isAdmin) return null;

  return (
    <DropdownMenu modal={false}>
      {children}
      <DropdownMenuContent {...props}>
        <DropdownMenuButton onSelect={onEdit}>Upravit</DropdownMenuButton>
        <DropdownMenuButton onSelect={sticky}>
          {item.sticky ? 'Odepnout' : 'Připnout'}
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={hide}>
          {item.isVisible ? 'Skrýt' : 'Zviditelnit'}
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={remove}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

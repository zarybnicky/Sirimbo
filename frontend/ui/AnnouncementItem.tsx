import {
    AnnouncementFragment,
    DeleteAnnouncementDocument,
    ToggleUpozorneniStickyDocument,
    ToggleUpozorneniVisibleDocument,
} from '@/graphql/Announcement';
import { Card, CardMenu } from '@/ui/Card';
import { CohortColorBoxes } from '@/ui/CohortColorBox';
import { useConfirm } from '@/ui/Confirm';
import { RichTextView } from '@/ui/RichTextView';
import { cn } from '@/ui/cn';
import { DropdownMenuButton } from '@/ui/dropdown';
import { fullDateFormatter } from '@/ui/format';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { useAuth } from '@/ui/use-auth';
import { useRouter } from 'next/router';
import React from 'react';
import { useMutation } from 'urql';

export const AnnouncementItem = ({ item, hideAll }: { item: AnnouncementFragment; hideAll?: boolean }) => {
  const router = useRouter();
  const confirm = useConfirm();
  const auth = useAuth();

  const [expanded, setExpanded] = React.useState(false);
  const [editing, setEditing] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);
  const close = React.useCallback(() => setExpanded(false), []);

  const hideMutation = useMutation(ToggleUpozorneniVisibleDocument)[1];
  const stickyMutation = useMutation(ToggleUpozorneniStickyDocument)[1];
  const deleteMutation = useMutation(DeleteAnnouncementDocument)[1];

  return editing ? (
    <Card>
      <AnnouncementForm id={item.id} data={item} onSuccess={() => setEditing(false)} />
    </Card>
  ) : (
    <Card
      onClick={expanded ? undefined : open}
      className={cn('group', !expanded && 'cursor-pointer')}
    >
      {auth.isAdmin && (
        <CardMenu>
          <DropdownMenuButton onClick={() => setEditing(true)}>
            Upravit
          </DropdownMenuButton>
          <DropdownMenuButton onClick={() => void stickyMutation({ id: item.id, sticky: !item.sticky })}>
            {item.sticky ? 'Odepnout' : 'Připnout'}
          </DropdownMenuButton>
          <DropdownMenuButton onClick={() => void hideMutation({ id: item.id, visible: false })}>
            {item.isVisible ? 'Skrýt' : 'Zviditelnit'}
          </DropdownMenuButton>
          <DropdownMenuButton
            onClick={async () => {
              await confirm({ description: `Opravdu chcete smazat příspěvek "${item.upNadpis}"?` });
              await deleteMutation({ id: item.id })
              router.replace('/nastenka');
            }}
          >
            Smazat
          </DropdownMenuButton>
        </CardMenu>
      )}

      <div className="text-neutral-12 text-sm flex flex-wrap items-baseline gap-4">
        <div>
          {[
            fullDateFormatter.format(new Date(item.upTimestampAdd)),
            item.userByUpKdo &&
              `${item.userByUpKdo?.uJmeno} ${item.userByUpKdo?.uPrijmeni}`,
          ]
            .filter(Boolean)
            .join(', ')}
        </div>
        <CohortColorBoxes
          items={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
            (x) => x.cohortByUpsIdSkupina,
          )}
        />
      </div>

      {hideAll ? (
        expanded ? (
          <>
            <h2 className="text-lg font-bold mb-4 cursor-pointer" onClick={close}>{item.upNadpis}</h2>
            <RichTextView value={item.upText} />
          </>
        ) : (
          <h2 className="text-lg font-bold">{item.upNadpis}</h2>
        )
      ) : (
        <div className="relative">
          <h2 className="text-lg font-bold mb-4">{item.upNadpis}</h2>
          <RichTextView className={expanded ? '' : 'ClampFade'} value={item.upText} />
          {!expanded && <div className="absolute bottom-0 text-accent-11 font-bold">Zobrazit více...</div>}
        </div>
      )}
    </Card>
  );
};

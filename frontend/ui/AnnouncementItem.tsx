import {
  AnnouncementFragment,
  ToggleUpozorneniStickyDocument,
  ToggleUpozorneniVisibleDocument,
} from '@app/graphql/Announcement';
import classNames from 'classnames';
import { fullDateFormatter } from '@app/ui/format';
import React from 'react';
import { Card, CardMenu } from './Card';
import { CohortColorBoxes } from './CohortColorBox';
import { RichTextView } from '@app/ui/RichTextView';
import { useAuth } from './use-auth';
import { useMutation } from 'urql';
import { DropdownMenuButton } from './dropdown';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { AnnouncementForm } from './AnnouncementForm';

export const AnnouncementItem = ({ item }: { item: AnnouncementFragment }) => {
  const [expanded, setExpanded] = React.useState(false);
  const [editOpen, setEditOpen] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);

  const { perms } = useAuth();
  const hideMutation = useMutation(ToggleUpozorneniVisibleDocument)[1];
  const stickyMutation = useMutation(ToggleUpozorneniStickyDocument)[1];

  return (
    <Card
      onClick={expanded ? undefined : open}
      className={classNames('group', !expanded && 'cursor-pointer')}
    >
      {perms.isAdmin && (
        <CardMenu>
          <Dialog open={editOpen} onOpenChange={setEditOpen}>
            <DialogTrigger asChild>
              <DropdownMenuButton onSelect={(e) => e.preventDefault()}>
                Upravit
              </DropdownMenuButton>
            </DialogTrigger>
            <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
              <AnnouncementForm id={item.id} data={item} onSuccess={() => setEditOpen(false)} />
            </DialogContent>
          </Dialog>
          <DropdownMenuButton onClick={() => void stickyMutation({ id: item.id, sticky: !item.sticky })}>
            {item.sticky ? 'Odepnout' : 'Připnout'}
          </DropdownMenuButton>
          <DropdownMenuButton onClick={() => void hideMutation({ id: item.id, visible: false })}>
            {item.isVisible ? 'Skrýt' : 'Zviditelnit'}
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
            (x) => x.skupinyByUpsIdSkupina,
          )}
        />
      </div>
      <h2 className="text-lg font-bold mb-4">{item.upNadpis}</h2>

      <RichTextView
        className={expanded ? '' : 'line-clamp-3'}
        value={item.upText}
      />
      {!expanded && <div className="text-accent-11 font-bold mt-3">Zobrazit více...</div>}
    </Card>
  );
};
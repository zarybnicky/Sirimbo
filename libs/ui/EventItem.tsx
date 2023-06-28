import * as React from 'react';
import { Card, CardMenu } from '@app/ui/Card';
import { fullDateFormatter } from '@app/ui/format-date';
import { EventWithItemsFragment } from '@app/graphql/Event';
import { ParticipationDialog } from './ParticipationForm';
import { RichTextView } from './RichTextView';
import { EventParticipantExport } from './EventParticipantExport';
import { useAuth } from '@app/ui/use-auth';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { DropdownMenuLink } from './dropdown';

interface Props {
  event: EventWithItemsFragment;
  expanded?: boolean;
}

export const EventItem = ({ event: item }: Props) => {
  const { user, perms } = useAuth();
  const total =
    (item.attendeeUsers?.nodes?.length ?? 0) +
    (item.attendeeExternals?.nodes?.length ?? 0);

  return (
    <Card className="break-inside-avoid">
      {perms.canEditEvent(item) && (
        <CardMenu>
          <DropdownMenuLink href={{ pathname: '/admin/akce/[id]', query: { id: item.id } }}>
            Upravit
          </DropdownMenuLink>
        </CardMenu>
      )}

      <div className="flex justify-between flex-wrap text-neutral-11">
        <div>
          {fullDateFormatter.formatRange(
            new Date(item.since || ''),
            new Date(item.until || ''),
          )}
        </div>
        <div>
          Zbývá {item.remainingSpots} míst z {item.capacity}
        </div>
      </div>
      <div className="text-2xl text-neutral-12">{item.name}</div>
      <div className="text-neutral-11">{item.locationText}</div>

      <div className="flex gap-1 flex-wrap my-4">
        <ParticipationDialog data={item} />

        {total > 0 && (
          <Dialog>
            <DialogTrigger className="button button-accent">
              Účastníci ({total})
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Účastníci</DialogTitle>
              {perms.canEditEvent(item) && <EventParticipantExport id={item.id} />}

              {!!item.attendeeUsers?.nodes?.length && <u>Členové</u>}
              {item.attendeeUsers?.nodes?.map((x) => (
                <div key={x.user?.uId}>
                  {x.user?.uJmeno} {x.user?.uPrijmeni}
                </div>
              ))}

              {!!item.attendeeExternals?.nodes?.length && <u>Externí</u>}
              {item.attendeeExternals?.nodes?.map((x, i) => (
                <div key={i}>
                  {x.firstName} {x.lastName}
                </div>
              ))}
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.summary && item.summary !== '[]' && (
        <RichTextView value={item.summary} />
      )}
      <RichTextView value={item.description} />
      {!!user && (
        <RichTextView value={item.descriptionMember} />
      )}
    </Card>
  );
};

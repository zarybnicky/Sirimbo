import * as React from 'react';
import { Card } from 'components/Card';
import { fullDateFormatter } from 'lib/format-date';
import { EventWithItemsFragment } from '@app/graphql/Event';
import { ParticipationDialog } from './ParticipationForm';
import { RichTextView } from './RichTextView';
import { EventParticipantExport } from './EventParticipantExport';
import { Event } from 'lib/entities';
import { useAuth } from 'lib/data/use-auth';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';

interface Props {
  event: EventWithItemsFragment;
  expanded?: boolean;
}

export const EventItem = ({ event }: Props) => {
  const { perms } = useAuth();
  const menu = Event.useMenu(event);
  const total =
    (event.attendeeUsers?.nodes?.length ?? 0) +
    (event.attendeeExternals?.nodes?.length ?? 0);

  return (
    <Card menu={menu} className="break-inside-avoid">
      <div className="flex justify-between flex-wrap text-neutral-11">
        <div>
          {fullDateFormatter.formatRange(
            new Date(event.since || ''),
            new Date(event.until || ''),
          )}
        </div>
        <div>
          Zbývá {event.remainingSpots} míst z {event.capacity}
        </div>
      </div>
      <div className="text-2xl text-neutral-12">{event.name}</div>
      <div className="text-neutral-11">{event.locationText}</div>

      <div className="flex gap-1 flex-wrap my-4">
        <ParticipationDialog data={event} />

        {total > 0 && (
          <Dialog>
            <DialogTrigger className="button button-accent">
              Účastníci ({total})
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Účastníci</DialogTitle>
              {perms.canEditEvent(event) && <EventParticipantExport id={event.id} />}

              {!!event.attendeeUsers?.nodes?.length && <u>Členové</u>}
              {event.attendeeUsers?.nodes?.map((x) => (
                <div key={x.user?.uId}>
                  {x.user?.uJmeno} {x.user?.uPrijmeni}
                </div>
              ))}

              {!!event.attendeeExternals?.nodes?.length && <u>Externí</u>}
              {event.attendeeExternals?.nodes?.map((x, i) => (
                <div key={i}>
                  {x.firstName} {x.lastName}
                </div>
              ))}
            </DialogContent>
          </Dialog>
        )}
      </div>

      {event.summary && event.summary !== '[]' && (
        <RichTextView value={event.summary} />
      )}
      <RichTextView value={event.description} />
    </Card>
  );
};
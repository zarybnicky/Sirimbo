import React from 'react';
import classNames from 'classnames';
import { fullDateFormatter } from '@app/ui/format-date';
import { MyEventsDocument } from '@app/graphql/Event';
import Link from 'next/link';
import { useQuery } from 'urql';
import { Card } from './Card';

type Props = JSX.IntrinsicAttributes & {
  selected?: string;
};

export function EventMemberList({ selected }: Props) {
  const [{ data }] = useQuery({query: MyEventsDocument});
  return (
    <>
      {data?.events?.nodes.map((event) => (
        <Link href={`/events/${event.id}`} key={event.id}>
          <Card className="flex flex-wrap justify-between items-center">
            <div
              className={classNames(
                'text-xl text-neutral-12',
                selected === event.id && 'font-bold',
              )}
            >
              {event.name}
            </div>
            <div className="text-sm text-neutral-11 flex flex-wrap gap-4">
              <div>
                {fullDateFormatter.formatRange(
                  new Date(event.since || ''),
                  new Date(event.until || ''),
                )}
              </div>
              <div>{event.locationText}</div>
              <div>
                Zbývá {event.remainingSpots} míst z {event.capacity}
              </div>
            </div>
          </Card>
        </Link>
      ))}
    </>
  );
}

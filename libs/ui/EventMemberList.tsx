import React from 'react';
import classNames from 'classnames';
import { fullDateFormatter } from '@app/ui/format-date';
import { EventInstanceRangeDocument } from '@app/graphql/Event';
import Link from 'next/link';
import { useQuery } from 'urql';
import { Card } from './Card';
import { startOf } from 'date-arithmetic';
import { TitleBar } from './TitleBar';

export function EventMemberList({ selected }: { selected?: string }) {
  const [today] = React.useState(() => startOf(new Date(), 'day'));
  const [{ data }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      start: today.toISOString(),
      type: 'CAMP',
    }
  });
  return (
    <>
      <TitleBar title="Nadcházející akce" />
      {data?.list?.reverse().map((instance) => (
        <Link href={`/akce/${instance.event!.id}`} key={instance.id}>
          <Card className="flex flex-wrap justify-between items-center">
            <div
              className={classNames(
                'text-xl text-neutral-12',
                selected === instance.event!.id && 'font-bold',
              )}
            >
              {instance.event!.name}
            </div>
            <div className="text-sm text-neutral-11 flex flex-wrap gap-4">
              <div>
                {fullDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </div>
              <div>{instance.event!.locationText}</div>
              <div>
                Zbývá {instance.event!.remainingPersonSpots} míst z {instance.event!.capacity}
              </div>
            </div>
          </Card>
        </Link>
      ))}
    </>
  );
}

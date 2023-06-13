import classNames from 'classnames';
import { fullDateFormatter } from 'lib/format-date';
import { MyEventsDocument } from '@app/graphql/Event';
import Link from 'next/link';
import { useQuery } from 'urql';
import { Card } from './Card';

type Props = {
  selected?: string;
};

export function EventMemberList({ selected }: Props) {
  const [{ data }] = useQuery({query: MyEventsDocument});
  return (
    <>
      {data?.events?.nodes.map((event) => (
        <Link href={{ pathname: '/events/[id]', query: {id: event.id} }} key={event.id}>
          <Card className="flex flex-wrap justify-between items-center">
            <div
              className={classNames(
                'text-xl text-red-500',
                selected === event.id && 'font-bold',
              )}
            >
              {event.name}
            </div>
            <div className="text-sm text-stone-600 flex flex-wrap gap-4">
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

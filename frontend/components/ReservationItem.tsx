import * as React from 'react';
import { MyReservationFragment } from '@app/graphql/Reservation';
import { Card } from 'components/Card';
import { shortDateFormatter } from 'lib/format-date';
import { formatCoupleName } from 'lib/format-name';
import { ReservationButton } from 'components/ReservationButton';
import { useAuth } from 'lib/data/use-auth';
import { Reservation } from 'lib/entities';

export const ReservationItem = ({ item }: { item: MyReservationFragment }) => {
  const { couple, perms} = useAuth();

  return (
    <Card
      menu={Reservation.useMenu(item)}
      className="group relative min-w-[200px] grid w-72 rounded-lg border-stone-200 border"
    >
      <div className="mb-0.5">
        <div className="font-bold">
          {shortDateFormatter.formatRange(new Date(item.nOd), new Date(item.nDo))}
        </div>
        <div className="text-xl">{item.userByNTrener?.fullName}</div>

        {item.nMaxPocetHod > 0 && (
          <div>
            <span className="text-stone-500">Maximálně hodin/pár: </span>
            <span className="text-lg">{item.nMaxPocetHod}</span>
          </div>
        )}
        <div className="text-sm text-stone-500">
          Zbývá {item.freeLessons} z {item.nPocetHod} nabízených
        </div>
      </div>

      {item.nabidkaItemsByNiIdRodic.nodes
        .filter((x) => x.niPartner !== couple?.id)
        .map((lesson, i) => (
          <div
            key={i}
            className="group flex gap-3 p-2.5 rounded-lg leading-4 text-sm tabular-nums"
          >
            <div>{formatCoupleName(lesson.paryByNiPartner)}</div>
            <div className="grow text-right">{lesson.niPocetHod}x</div>
          </div>
        ))}
      {(perms.canMakeReservation(item) || item.myLessons) && (
        <ReservationButton item={item} />
      )}
    </Card>
  );
};

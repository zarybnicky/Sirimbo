import * as React from 'react';
import { MyReservationFragment } from '@app/graphql/Reservation';
import { Card, CardMenu } from '@app/ui/Card';
import { shortDateFormatter } from '@app/ui/format-date';
import { formatCoupleName } from '@app/ui/format-name';
import { ReservationButton } from '@app/ui/ReservationButton';
import { useAuth } from '@app/ui/use-auth';
import { DropdownMenuLink } from './dropdown';

export const ReservationItem = ({ item }: { item: MyReservationFragment }) => {
  const { couple, perms } = useAuth();

  return (
    <Card className="group relative min-w-[200px] grid w-72 rounded-lg border-neutral-7 border">
      {perms.canEditReservation(item) && (
        <CardMenu>
          <DropdownMenuLink href={`/admin/nabidka/${item.id}`}>
            Upravit
          </DropdownMenuLink>
        </CardMenu>
      )}

      <div className="mb-0.5">
        <div className="font-bold">
          {shortDateFormatter.formatRange(new Date(item.nOd), new Date(item.nDo))}
        </div>
        <div className="text-xl">{item.userByNTrener?.fullName}</div>

        {item.nMaxPocetHod > 0 && (
          <div>
            <span className="text-neutral-11">Maximálně hodin/pár: </span>
            <span className="text-lg">{item.nMaxPocetHod}</span>
          </div>
        )}
        <div className="text-sm text-neutral-11">
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
import * as React from 'react';
import { usePermissions } from 'lib/data/use-permissions';
import { MyReservationFragment } from 'lib/graphql/Reservation';
import { Dropdown } from 'components/Dropdown';
import { Card } from 'components/Card';
import { MoreVertical } from 'react-feather';
import { shortDateFormatter } from 'lib/format-date';
import { formatCoupleName } from 'lib/format-name';
import classNames from 'classnames';
import { ReservationButton } from 'components/ReservationButton';
import { useAuth } from 'lib/data/use-auth';

export const ReservationItem = ({ item }: { item: MyReservationFragment; }) => {
  const { couple } = useAuth();
  const perms = usePermissions();

  return (
    <div className="group relative min-w-[200px]">
      {perms.canEditReservation(item) && (
        <div className="absolute right-2 top-2">
          <Dropdown align="end"
            button={<MoreVertical className="text-stone-500 w-6 invisible ui-open:visible group-hover:visible" />}
            options={[
              { title: "Upravit", href: `/admin/nabidka/${item.id}` },
              { title: "Upravit rezervace", href: `/admin/nabidka/detail/${item.id}` },
            ]}
          />
        </div>
      )}

      <div className="mb-0.5">
        <div className="font-bold">{shortDateFormatter.formatRange(new Date(item.nOd), new Date(item.nDo))}</div>
        <div className="text-xl">{item.userByNTrener?.fullName}</div>

        {item.nMaxPocetHod > 0 && (
          <div>
            <span className="text-stone-500">Maximálně hodin/pár: </span>
            <span className="text-lg">{item.nMaxPocetHod}</span>
          </div>
        )}
        <div>
          <span className="text-stone-500">Volných hodin: </span>
          <span className="text-lg">{item.freeLessons} z {item.nPocetHod} nabízených</span>
        </div>
      </div>

      <Card className="grid mx-auto w-72 rounded-lg border-stone-200 border">
        {item.nabidkaItemsByNiIdRodic.nodes.filter(x => x.niPartner !== couple?.id).map((lesson, i) => (
          <div key={i} className={classNames(
            "group flex gap-3 p-2.5 rounded-lg",
            "leading-4 text-sm tabular-nums",
          )}>
            <div>{formatCoupleName(lesson.paryByNiPartner)}</div>
            <div className="grow text-right">{lesson.niPocetHod}x</div>
          </div>
        ))}
        {(perms.canMakeReservation(item) || item.myLessons) && (
          <ReservationButton item={item} />
        )}
      </Card>
    </div>
  );
};

import * as React from 'react';
import { DateRange } from './DateRange';
import { usePermissions } from 'lib/data/use-permissions';
import { ReservationFragment, useReservationListQuery } from 'lib/graphql';
import { Dropdown } from 'components/Dropdown';

const ReservationView = (x: ReservationFragment) => {
  const perms = usePermissions();
  const header = <div className="trenink-header">
    <div className="title">
      {x.userByNTrener?.uJmeno} {x.userByNTrener?.uPrijmeni}
      {perms.canEditReservation(x) && (
        <Dropdown
          button={<img alt="Upravit" width="16" src="/style/icon-gear.png" />}
          options={[
            { title: "Upravit", href: `/admin/nabidka/edit/${x.nId}` },
            { title: "Upravit rezervace", href: `/admin/nabidka/detail/${x.nId}` },
          ]}
        />
      )}
    </div>
    <div className="date"><DateRange from={x.nOd} to={x.nDo} noYear /></div>
    {x?.nMaxPocetHod > 0 && <div>
      <span className="little"> Maximálně hodin/pár: </span>
      <span className="nadpis">{x?.nMaxPocetHod}</span>
    </div>}
    <div>
      <span className="little">Volných hodin: </span>
      <span className="nadpis">
        {x?.nPocetHod - (x.nabidkaItemsByNiIdRodic?.nodes || []).reduce((x, y) => x + y.niPocetHod, 0)}
        {" z "}
        {x?.nPocetHod} nabízených
      </span>
    </div>
  </div>;
  const content = <table className="nocolor" style={{ width: '100%' }}>
    <thead>
      <tr><th>Tanečník</th><th>Počet hodin</th></tr>
    </thead>
    <tbody>
      {x.nabidkaItemsByNiIdRodic?.nodes?.map((item, i) => <tr key={i}>
        <td>{item.paryByNiPartner?.userByPIdPartner?.uJmeno} {item.paryByNiPartner?.userByPIdPartner?.uPrijmeni}</td>
        <td>{item.niPocetHod}</td>
      </tr>)}
    </tbody>
  </table>;

  return <div className="col-12 col-md-6 col-lg-4 pb-2">
    <div className="widget">
      <div className="widget-title text-center">{header}</div>
      <div className="widget-content">{content}</div>
    </div>
  </div>;
}

export function ReservationSelect() {
  const { data: reservations } = useReservationListQuery({ limit: 30, offset: 0 });
  const [reservation, setReservation] = React.useState<ReservationFragment | undefined>();
  const onChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
    setReservation(reservations?.nabidkas?.nodes?.find(x => x.nId === event.target.value));
  };

  return <div>
    <select className='team-selection' value={reservation?.nId || 'none'} onChange={onChange}>
      <option value='none'> --vyberte nabídku-- </option>
      {reservations?.nabidkas?.nodes?.map(x => <option value={x.nId} key={x.nId}>
        <DateRange from={x.nOd} to={x.nDo} />
        {` - ${x.userByNTrener?.uJmeno} ${x.userByNTrener?.uPrijmeni}`}
      </option>)}
    </select>
    {reservation ? ReservationView(reservation) : null}
  </div>;
}

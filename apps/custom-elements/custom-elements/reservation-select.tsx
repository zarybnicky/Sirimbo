import React from 'react';
import { formatDateRange } from './date';
import { useQuery } from 'urql';
import {
  ReservationFragment,
  ReservationDetailListDocument,
} from '@app/graphql/Reservation';
import {formatCoupleName} from '@app/ui/format-name';

export default function ReservationSelect() {
  const [{ data: reservations }] = useQuery({ query: ReservationDetailListDocument });
  const [reservation, setReservation] = React.useState<ReservationFragment | undefined>();
  const onChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setReservation(
      reservations?.nabidkas?.nodes?.find((x) => x.id === event.target.value),
    );
  };

  return (
    <div>
      <select
        className="team-selection"
        value={reservation?.id || 'none'}
        onChange={onChange}
      >
        <option value="none"> --vyberte nabídku-- </option>
        {(reservations?.nabidkas?.nodes || []).map((x) => (
          <option value={x.id} key={x.id}>
            {`${formatDateRange(x.nOd, x.nDo)} - ${x.userByNTrener?.uJmeno} ${
              x.userByNTrener?.uPrijmeni
            }`}
          </option>
        ))}
      </select>
      {!reservation ? null : (
        <div className="col-12 col-md-6 col-lg-4 pb-2">
          <div className="widget">
            <div className="widget-title text-center">
              <div className="trenink-header">
                <div className="title">
                  {reservation.userByNTrener?.uJmeno}{' '}
                  {reservation.userByNTrener?.uPrijmeni}
                </div>
                <div className="date">
                  {formatDateRange(reservation.nOd, reservation.nDo, '1')}
                </div>
                {reservation.nMaxPocetHod > 0 && (
                  <div>
                    <span className="little"> Maximálně hodin/pár: </span>
                    <span className="nadpis">{reservation.nMaxPocetHod}</span>
                  </div>
                )}
                <div>
                  <span className="little">Volných hodin: </span>
                  <span className="nadpis">
                    {reservation.nPocetHod -
                      (reservation.nabidkaItemsByNiIdRodic.nodes || []).reduce(
                        (x, y) => x + y.niPocetHod,
                        0,
                      )}
                    {' z '}
                    {reservation.nPocetHod} nabízených
                  </span>
                </div>
              </div>
            </div>

            <div className="widget-content">
              <table className="nocolor" style={{ width: '100%' }}>
                <thead>
                  <tr>
                    <th>Tanečník</th>
                    <th>Počet hodin</th>
                  </tr>
                </thead>
                <tbody>
                  {(reservation.nabidkaItemsByNiIdRodic.nodes || []).map((item) => (
                    <tr>
                      <td>{formatCoupleName(item.paryByNiPartner)}</td>
                      <td>{item.niPocetHod}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

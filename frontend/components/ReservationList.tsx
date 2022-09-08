import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button } from '@material-ui/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { Link } from 'react-router-dom';
import { Pagination } from '@material-ui/lab';
import { $, NabidkasOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';
import { useAuth } from '../data/use-auth';
import { DateRange } from './DateRange';

export const NabidkaAdminQuery = Selector('Query')({
  nabidkas: [
    { first: $`limit`, offset: $`offset`, orderBy: [NabidkasOrderBy.N_OD_DESC] },
    {
      nodes: {
        nDo: true,
        nId: true,
        nLock: true,
        nMaxPocetHod: true,
        nOd: true,
        nPocetHod: true,
        nTimestamp: true,
        nTrener: true,
        nVisible: true,
        nabidkaItemsByNiIdRodic: [{}, {
          nodes: {
            niPocetHod: true,
            niPartner: true,
            niLock: true,
            paryByNiPartner: {
              userByPIdPartner: {
                uJmeno: true,
                uPrijmeni: true,
                uId: true,
              },
            },
          },
        }],
        userByNTrener: {
          uJmeno: true,
          uPrijmeni: true,
          uId: true,
        },
      },
      totalCount: true,
    },
  ],
});

const ToggleVisibleNabidka = Selector("Mutation")({
  updateNabidka: [
    { input: { patch: { nVisible: $`visible` }, nId: $`id` } },
    {
      nabidka: {
        nId: true,
      },
    },
  ],
});

export function ReservationAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useTypedQuery(NabidkaAdminQuery, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const [toggleVisible] = useTypedMutation(ToggleVisibleNabidka, {
    onCompleted: () => refetch(),
  });
  const total = data?.nabidkas?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.nabidkas?.nodes.map((a) => <tr key={a.nId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
              <Button {...bindTrigger(popupState)}>
                {a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}
              </Button>
              <Menu {...bindMenu(popupState)} getContentAnchorEl={null}>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/nabidka/edit/${a.nId}`}>
                  Upravit
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/nabidka/detail/${a.nId}`}>
                  Upravit lekce
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/nabidka/duplicate/${a.nId}`}>
                  Duplikovat
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/nabidka/remove/${a.nId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        </td>
        <td><DateRange from={a.nOd} to={a.nDo} /></td>
        <td>
          <Checkbox checked={a.nVisible || false} onChange={() => toggleVisible({
            variables: { id: a.nId, visible: !a.nVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/nabidka/add" className="btn btn-primary">Nová nabídka</a>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}

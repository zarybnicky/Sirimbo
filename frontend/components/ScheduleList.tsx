import * as React from 'react';
import format from 'date-fns/format';
import { Checkbox, Menu, MenuItem, Button } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import Link from 'next/link';
import { Pagination } from '@mui/lab';
import { $, RozpisOrderBy, Selector } from 'lib/zeus';
import { useTypedQuery, useTypedMutation } from 'lib/zeus/apollo';
import { useAuth } from 'lib/data/use-auth';

export const ScheduleListQuery = Selector('Query')({
  rozpis: [
    { first: $`limit`, offset: $`offset`, orderBy: [RozpisOrderBy.R_DATUM_DESC] },
    {
      nodes: {
        rDatum: true,
        rId: true,
        rKde: true,
        rLock: true,
        rTimestamp: true,
        rTrener: true,
        rVisible: true,
        userByRTrener: {
          uId: true,
          uJmeno: true,
          uPrijmeni: true,
        },
        rozpisItemsByRiIdRodic: [{}, {
          nodes: {
            riDo: true,
            riOd: true,
            riId: true,
            riPartner: true,
          },
        }],
      },
      totalCount: true,
    }
  ],
});

export const ToggleScheduleVisible = Selector('Mutation')({
  updateRozpi: [
    { input: { patch: { rVisible: $`visible` }, rId: $`id` } },
    {
      rozpi: {
        rId: true,
      }
    }
  ],
});

export function RozpisAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const { data, refetch } = useTypedQuery(ScheduleListQuery, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const [toggleVisible] = useTypedMutation(ToggleScheduleVisible, {
    onCompleted: () => refetch(),
  });
  const total = data?.rozpis?.totalCount || 0;

  const list = (!user || !total) ? null : <table>
    <thead>
      <tr>
        <th>Trenér</th>
        <th>Datum</th>
        <th>Místo</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data?.rozpis?.nodes?.map((a) => <tr key={a.rId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
              <Button {...bindTrigger(popupState)}>
                {a.userByRTrener?.uJmeno} {a.userByRTrener?.uPrijmeni}
              </Button>
              <Menu {...bindMenu(popupState)} getContentAnchorEl={null}>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/rozpis/edit/${a.rId}`}>
                  Upravit
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/rozpis/detail/${a.rId}`}>
                  Upravit lekce
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/rozpis/duplicate/${a.rId}`}>
                  Duplikovat
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/rozpis/remove/${a.rId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        </td>
        <td>{format(new Date(a.rDatum), 'd. M. y')}</td>
        <td>{a.rKde}</td>
        <td>
          <Checkbox checked={a.rVisible || false} onChange={() => toggleVisible({
            variables: { id: a.rId, visible: !a.rVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <Link href="/admin/rozpis/add" passHref>
      <a className="btn btn-primary">Nový rozpis</a>
    </Link>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}

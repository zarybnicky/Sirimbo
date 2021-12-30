import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button } from '@material-ui/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { Link } from 'react-router-dom';
import { Pagination } from '@material-ui/lab';
import { $, AkcesOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';
import { DateRange } from './DateRange';

export const EventListQuery = Selector('Query')({
  allAkces: [
    { first: $`limit`, offset: $`offset`, orderBy: [AkcesOrderBy.A_OD_DESC] },
    {
      nodes: {
        aDo: true,
        aId: true,
        aInfo: true,
        aDokumenty: true,
        aJmeno: true,
        aKapacita: true,
        aKde: true,
        aLock: true,
        aOd: true,
        aTimestamp: true,
        aVisible: true,
        akceItemsByAiIdRodic: [{}, {
          nodes: {
            aiId: true,
            userByAiUser: {
              uJmeno: true,
              uPrijmeni: true,
              uId: true,
            },
          },
          totalCount: true,
        }],
      },
      totalCount: true,
    },
  ],
});

export const ToggleEventVisible = Selector('Mutation')({
  updateAkceByAId: [
    { input: { aId: $`id`, akcePatch: { aVisible: $`visible` } } },
    {
      akce: {
        aId: true,
      },
    },
  ],
});

export function EventList() {
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(1);
  const [total, setTotal] = React.useState(0);
  const { data, refetch } = useTypedQuery(EventListQuery, {
    variables: { limit, offset: (page - 1) * limit },
    onCompleted: (data) => {
      const total = data.allAkces?.totalCount;
      total && setTotal(total);
    },
  });
  const [toggleVisible] = useTypedMutation(ToggleEventVisible, {
    onCompleted: () => refetch(),
  });

  const list = !data?.allAkces?.nodes.length ? null : <table>
    <thead>
      <tr>
        <th>Jméno</th>
        <th>Datum</th>
        <th>Kapacita</th>
        <th>Viditelný</th>
      </tr>
    </thead>
    <tbody>
      {data!.allAkces.nodes.map((a) => <tr key={a.aId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <React.Fragment>
              <Button {...bindTrigger(popupState)}>{a.aJmeno}</Button>
              <Menu {...bindMenu(popupState)} getContentAnchorEl={null}>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/akce/edit/${a.aId}`}>
                  Upravit
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/akce/detail/${a.aId}`}>
                  Upravit účastníky
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/akce/dokumenty/${a.aId}`}>
                  Upravit dokumenty
                </MenuItem>
                <MenuItem button onClick={popupState.close} component={Link} to={`/admin/akce/remove/${a.aId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        </td>
        <td><DateRange from={a.aOd} to={a.aDo} /></td>
        <td>{a.akceItemsByAiIdRodic.totalCount || 0}/{a.aKapacita}</td>
        <td>
          <Checkbox checked={a.aVisible} onChange={() => toggleVisible({
            variables: { id: a.aId, visible: !a.aVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <React.Fragment>
    <a href="/admin/akce/add" className="btn btn-primary">Přidat</a>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </React.Fragment>;
}

import * as React from 'react';
import { Checkbox, Menu, MenuItem, Button, Pagination } from '@mui/material';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { $, NabidkasOrderBy, Selector } from 'lib/zeus';
import { useAuth } from 'lib/data/use-auth';
import { DateRange } from 'components/DateRange';
import { NextLinkComposed } from 'components/Link';
import { useTypedMutation, useTypedQuery } from 'lib/query';

export const NabidkaAdminQuery = Selector('Query')({
  nabidkas: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [NabidkasOrderBy.N_OD_DESC],
    },
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
    {
      input: {
        nId: $('id', 'BigInt!'),
        patch: {
          nVisible: $('visible', 'Boolean!'),
        },
      }
    },
    {
      nabidka: {
        nId: true,
      },
    },
  ],
});

export default function ReservationAdminList() {
  const { user } = useAuth();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useTypedQuery(['nabidkaAdmin'], NabidkaAdminQuery, {}, {
    variables: { limit, offset: (page - 1) * limit },
  });
  const { mutate: toggleVisible } = useTypedMutation(['toggleNabidka'], ToggleVisibleNabidka, {
    onSuccess: () => refetch(),
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
      {data?.nabidkas?.nodes?.map((a) => <tr key={a.nId}>
        <td>
          <PopupState variant="popover">
            {(popupState) => <>
              <Button {...bindTrigger(popupState)}>
                {a.userByNTrener?.uJmeno} {a.userByNTrener?.uPrijmeni}
              </Button>
              <Menu {...bindMenu(popupState)}>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/edit/${a.nId}`}>
                  Upravit
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/detail/${a.nId}`}>
                  Upravit lekce
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/duplicate/${a.nId}`}>
                  Duplikovat
                </MenuItem>
                <MenuItem onClick={popupState.close} component={NextLinkComposed} href={`/admin/nabidka/remove/${a.nId}`}>
                  Odstranit
                </MenuItem>
              </Menu>
            </>}
          </PopupState>
        </td>
        <td><DateRange from={a.nOd} to={a.nDo} /></td>
        <td>
          <Checkbox checked={a.nVisible} onChange={() => toggleVisible({
            variables: { id: a.nId, visible: !a.nVisible },
          })} />
        </td>
      </tr>)}
    </tbody>
  </table>;

  return <>
    <NextLinkComposed href="/admin/nabidka/add" className="btn btn-primary">Nová nabídka</NextLinkComposed>
    {list}
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}

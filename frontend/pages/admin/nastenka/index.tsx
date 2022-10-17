import { useAnnouncementListQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import React from "react";

export default function AnnouncementAdminList() {
  useRequireUserLoggedIn();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { data } = useAnnouncementListQuery({ limit, offset: (page - 1) * limit });

  return <>
    <NextLinkComposed href="/admin/nastenka/add" className="btn btn-primary">Přidat</NextLinkComposed>
    <table>
      <thead>
        <tr>
          <th>Jméno</th>
          <th>Datum</th>
          <th>Kapacita</th>
          <th>Viditelný</th>
        </tr>
      </thead>
      <tbody>
        {data?.akces?.nodes?.map((a) => <tr key={a.aId}>
          <td>
            <Dropdown
              button={<>{a.aJmeno}</>}
              options={[
                { title: 'Upravit', href: `/admin/rozpis/edit/${a.aId}` },
                { title: 'Upravit účastníky', href: `/admin/rozpis/detail/${a.aId}` },
                { title: 'Upravit dokumenty', href: `/admin/rozpis/detail/${a.aId}` },
                { title: 'Odstranit', href: `/admin/rozpis/remove/${a.aId}` },
              ]}
            />
          </td>
          <td><DateRange from={a.aOd} to={a.aDo} /></td>
          <td>{a.akceItemsByAiIdRodic.totalCount || 0}/{a.aKapacita}</td>
          <td>
            <Checkbox checked={a.aVisible} onChange={() => toggleVisible({ id: a.aId, visible: !a.aVisible })} />
          </td>
        </tr>)}
      </tbody>
    </table>
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}

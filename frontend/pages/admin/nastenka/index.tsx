import { Pagination } from "@mui/material";
import { Dropdown } from "components/Dropdown";
import { NextLinkComposed } from "components/Link";
import { useAnnouncementListQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import React from "react";

export default function AnnouncementAdminList() {
  useRequireUserLoggedIn();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { data } = useAnnouncementListQuery({ limit, offset: (page - 1) * limit });
  const total = data?.upozornenis?.totalCount || 0;

  return <>
    <NextLinkComposed href="/admin/nastenka/add" className="btn btn-primary">Přidat</NextLinkComposed>
    <table>
      <tbody>
        {data?.upozornenis?.nodes?.map((a) => <tr key={a.upId}>
          <td>
            <Dropdown
              button={<img alt="Upravit" width="14" src="/style/icon-gear.png" />}
              options={[
                { title: 'Upravit', href: `/admin/nastenka/edit/${a.upId}` },
                { title: 'Odstranit', href: `/admin/nastenka/remove/${a.upId}` },
              ]}
            />
          </td>
          <td></td>
        </tr>)}
      </tbody>
    </table>
    <Pagination count={Math.ceil(total / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </>;
}

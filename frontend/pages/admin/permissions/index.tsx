import * as React from 'react';
import { NextLinkComposed } from 'components/Link';
import { Dropdown } from 'components/Dropdown';
import { usePermissionListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';

export default function PermissionAdminList() {
  useRequireUserLoggedIn();
  const { data } = usePermissionListQuery();

  return <>
    <NextLinkComposed href="/admin/permissions/add" className="btn btn-primary">Nová role</NextLinkComposed>
    <table>
      <tbody>
        {data?.permissions?.nodes?.map((a) => <tr key={a.peId}>
          <td>
            <Dropdown
              button={<>{a.peName}</>}
              options={[
                { title: 'Upravit', href: `/admin/permissions/edit/${a.peId}` },
                { title: 'Odstranit', href: `/admin/aktuality/remove/${a.peId}` },
              ]}
            />
          </td>
          <td>{a.peDescription}</td>
        </tr>)}
      </tbody>
    </table>
  </>;
}

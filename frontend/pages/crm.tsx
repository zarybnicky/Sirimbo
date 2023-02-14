import * as React from 'react';
import { useActiveProspectsQuery } from 'lib/graphql/Crm';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatFullDate } from 'lib/format-date';
import { Item } from 'components/layout/Item';

export default function CrmPage() {
  const { data } = useActiveProspectsQuery();

  return <Item className="col-feature">
    <Item.Titlebar title="Chci tančit!" />

    <table>
      <thead>
        <tr>
          <th>Jméno</th>
          <th>E-mail</th>
          <th>Rok narození</th>
          <th>Zdroj</th>
          <th>Poslední aktivita</th>
        </tr>
      </thead>
      <tbody>
        {(data?.activeProspects?.nodes || []).map((row, i) => (
          <tr key={i}>
            <td>{row.data?.name} {row.data?.surname}</td>
            <td>{row.data?.email}</td>
            <td>{row.data?.phone}</td>
            <td>{row.data?.yearofbirth}</td>
            <td>{row.cohort}</td>
            <td>{row.updatedAt ? formatFullDate(new Date(row.updatedAt)) : ''}</td>
          </tr>
        ))}
      </tbody>
    </table>
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_ADMIN,
);

'use client';

import { TenantsDocument } from '@/graphql/Tenant.ts';
import { fullDateFormatter } from '@/ui/format';
import { Table } from '@/ui/Table';
import { useQuery } from 'urql';

export function Tenants() {
  const [{ data, fetching, error }] = useQuery({ query: TenantsDocument });
  const tenants = data?.tenantsList;

  return (
    <Table
      columns={['ID', 'Jméno', 'Členové', 'Trenéři', 'Správci', 'Vytvořeno']}
      busy={fetching}
      error={error}
    >
      {tenants?.map((tenant) => (
        <tr key={tenant.id}>
          <td>{tenant.id}</td>
          <th scope="row">
            <a
              href={`/api/auth/handoff?${new URLSearchParams({ tenant: tenant.id, from: '/tanecni-klub' })}`}
              className="text-left text-accent-11 underline"
            >
              {tenant.name}
            </a>
          </th>
          <td>{tenant.memberships.totalCount}</td>
          <td>{tenant.trainers.totalCount}</td>
          <td>{tenant.administrators.totalCount}</td>
          <td>
            {tenant.createdAt
              ? fullDateFormatter.format(new Date(tenant.createdAt))
              : '—'}
          </td>
        </tr>
      ))}
    </Table>
  );
}

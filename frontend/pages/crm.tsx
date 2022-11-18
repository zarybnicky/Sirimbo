import * as React from 'react';
import { DataGrid } from '@mui/x-data-grid';
import { useActiveProspectsQuery } from 'lib/graphql/Crm';
import { Layout } from 'components/layout/Layout';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatFullDate } from 'lib/format-date';

export default function CrmPage() {
  const { data } = useActiveProspectsQuery();

  return <div className="container mx-auto max-w-5xl" style={{ padding: '2rem 0' }}>
    <h4 className="text-lg font-bold mb-2">Zájemci</h4>

    <DataGrid
      autoHeight={true}
      rows={data?.activeProspects?.nodes || []}
      columns={[
        {
          field: 'name', headerName: 'Jméno', flex: 1,
          valueGetter: ({ row }) => `${row.data?.name} ${row.data?.surname}`,
        },
        {
          field: 'email', headerName: 'E-mail', flex: 1,
          valueGetter: ({ row }) => row.data?.email,
        },
        {
          field: 'phone', headerName: 'Telefon', flex: 1,
          valueGetter: ({ row }) => row.data?.phone,
        },
        {
          field: 'birthyear', headerName: 'Rok narození', flex: 1,
          valueGetter: ({ row }) => row.data?.yearofbirth,
        },
        { field: 'cohort', headerName: 'Zdroj', flex: 1 },
        {
          field: 'updatedAd', headerName: 'Poslední aktivita', flex: 1,
          renderCell: ({ row }) => row.updatedAt ? formatFullDate(new Date(row.updatedAt)) : '',
        },
      ]}
    />
  </div>;
}

CrmPage.getLayout = (page: React.ReactElement) => <Layout>{page}</Layout>;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_ADMIN,
);

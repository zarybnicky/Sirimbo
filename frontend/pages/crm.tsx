import * as React from 'react';
import { format } from 'date-fns';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid } from '@mui/x-data-grid';
import { useActiveProspectsQuery } from 'lib/graphql/Crm';

export default function CrmPage() {
  useRequireUserLoggedIn()
  const { data } = useActiveProspectsQuery();

  return <div className="container mx-auto max-w-5xl" style={{ padding: '2rem 0' }}>
    <h4 className="text-right">Zájemci</h4>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.id!}
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
          renderCell: ({ row }) => row.updatedAt ? format(new Date(row.updatedAt), 'd. M. y') : '',
        },
      ]}
    />
  </div>;
}

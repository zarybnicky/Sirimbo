import * as React from 'react';
import { Button } from 'components/Button';
import { useDeleteVideoSourceMutation, useVideoSourceListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import format from 'date-fns/format';
import { useRouter } from 'next/router';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';

export default function VideoSourceList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useVideoSourceListQuery();
  const { mutateAsync: doDelete } = useDeleteVideoSourceMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/video/source/add">Přidat zdroj</Button>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.vsId}
      rows={data?.videoSources?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/video/source/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat zdroj videí" />,
          ]
        },
        { field: 'vsTitle', headerName: 'Zdroj', flex: 1 },
        {
          field: 'date', headerName: 'Poslední kontrola', flex: 1,
          renderCell: ({ row }) => row.vsCreatedAt ? format(new Date(row.vsCreatedAt), 'd. M. y') : '',
        },
        {
          field: 'checked', headerName: 'Poslední kontrola', flex: 1,
          renderCell: ({ row }) => row.vsLastChecked ? format(new Date(row.vsLastChecked), 'd. M. y') : '',
        },
      ]}
    />
  </div>;
}

import * as React from 'react';
import { Button } from 'components/Button';
import { useDeleteVideoMutation, useVideoListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { format } from 'date-fns';
import { useRouter } from 'next/router';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';

export default function VideoList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useVideoListQuery();
  const { mutateAsync: doDelete } = useDeleteVideoMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/video/add">Přidat video</Button>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.vId}
      rows={data?.videos?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/video/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat video" />,
          ]
        },
        { field: 'vName', headerName: 'Jméno', flex: 1 },
        { field: 'vId', headerName: 'ID videa', flex: 1 },
        {
          field: 'checked', headerName: 'Přidáno', flex: 1,
          renderCell: ({ row }) => row.vCreatedAt ? format(new Date(row.vCreatedAt), 'd. M. y') : '',
        },
      ]}
    />
  </div>;
}

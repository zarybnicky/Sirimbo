import * as React from 'react';
import { Button } from 'components/Button';
import { useDeleteVideoMutation, useVideoListQuery } from 'lib/graphql/Video';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { useRouter } from 'next/router';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function VideoList() {
  const router = useRouter();
  const { data, refetch } = useVideoListQuery();
  const { mutateAsync: doDelete } = useDeleteVideoMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/video/add">Přidat video</Button>

    <DataGrid
      autoHeight={true}
      rows={data?.videos?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/video/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat video" />,
          ]
        },
        { field: 'vName', headerName: 'Jméno', flex: 1 },
        { field: 'vId', headerName: 'ID videa', flex: 1 },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peGalerie, PermissionLevel.P_OWNED,
);

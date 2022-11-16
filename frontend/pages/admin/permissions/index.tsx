import * as React from 'react';
import { useDeleteRoleMutation, useRoleListQuery } from 'lib/graphql/Roles';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { useRouter } from 'next/router';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';

export default function PermissionAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useRoleListQuery();
  const { mutateAsync: doDelete } = useDeleteRoleMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/permissions/add">Nová role</Button>
    <DataGrid
      autoHeight={true}
      getRowId={row => row.peId}
      rows={data?.permissions?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/permissions/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat uživatelskou roli" />,
          ]
        },
        { field: 'peName', headerName: 'Jméno', flex: 1, },
        { field: 'peDescription', headerName: 'Popis', flex: 1, },
      ]}
    />
  </div>;
}

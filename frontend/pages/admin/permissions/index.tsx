import * as React from 'react';
import { NextLinkComposed } from 'components/Link';
import { useDeleteRoleMutation, usePermissionListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { useRouter } from 'next/router';
import { Container } from '@mui/material';
import { DeleteButton } from 'components/DeleteButton';

export default function PermissionAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = usePermissionListQuery();
  const { mutateAsync: doDelete } = useDeleteRoleMutation({
    onSuccess: () => refetch(),
  });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/permissions/add" className="btn btn-primary">Nová role</NextLinkComposed>
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
            <DeleteButton
              key="delete" title="smazat uživatelskou roli"
              params={{ id: id.toString() }} onDelete={doDelete}
            />,
          ]
        },
        { field: 'peName', headerName: 'Jméno', flex: 1, },
        { field: 'peDescription', headerName: 'Popis', flex: 1, },
      ]}
    />
  </Container>;
}

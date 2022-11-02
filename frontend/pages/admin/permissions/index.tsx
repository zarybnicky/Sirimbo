import * as React from 'react';
import { NextLinkComposed } from 'components/Link';
import { useDeleteRoleMutation, usePermissionListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useSnackbar } from 'notistack';
import { useConfirm } from 'material-ui-confirm';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import { useRouter } from 'next/router';
import { Container } from '@mui/material';

export default function PermissionAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const confirm = useConfirm();
  const { enqueueSnackbar } = useSnackbar();
  const { data, refetch } = usePermissionListQuery();
  const { mutateAsync: doDeleteItem } = useDeleteRoleMutation({
    onSuccess: () => refetch(),
  });
  const deleteItem = React.useCallback(async (id: string) => {
    await confirm({ description: 'Opravdu chcete smazat uživatelskou roli?' });
    try {
      await doDeleteItem({ id });
    } catch (e) {
      if (e instanceof Error) {
        enqueueSnackbar(e.message, { variant: 'error' });
      } else {
        enqueueSnackbar('Nepodařilo se smazat položku', { variant: 'error' });
      }
    }
  }, [])

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
            <GridActionsCellItem key="delete"
              icon={<DeleteIcon />}
              onClick={() => deleteItem(id.toString())}
              label="Odstranit"
            />,
          ]
        },
        { field: 'peName', headerName: 'Jméno', flex: 1, },
        { field: 'peDescription', headerName: 'Popis', flex: 1, },
      ]}
    />
  </Container>;
}

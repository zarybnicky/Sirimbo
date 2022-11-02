import * as React from 'react';
import { Container } from '@mui/material';
import { NextLinkComposed } from 'components/Link';
import { useDeleteVideoSourceMutation, useVideoSourceListQuery } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import format from 'date-fns/format';
import { useRouter } from 'next/router';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import { useConfirm } from 'material-ui-confirm';
import { useSnackbar } from 'notistack';

export default function VideoSourceList() {
  useRequireUserLoggedIn();
  const confirm = useConfirm();
  const router = useRouter();
  const { enqueueSnackbar } = useSnackbar();
  const { data, refetch } = useVideoSourceListQuery();
  const { mutateAsync: doDeleteItem } = useDeleteVideoSourceMutation({
    onSuccess: () => refetch(),
  });

  const deleteItem = React.useCallback(async (id: string) => {
    await confirm({ description: 'Opravdu chcete smazat zdroj videí?' });
    try {
      await doDeleteItem({ id });
    } catch (e) {
      if (e instanceof Error) {
        enqueueSnackbar(e.message, { variant: 'error' });
      } else {
        enqueueSnackbar('Nepodařilo se smazat položku', { variant: 'error' });
      }
    }
  }, []);

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/video/source/add" className="btn btn-outline-primary">Přidat zdroj</NextLinkComposed>

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
            <GridActionsCellItem key="delete"
              icon={<DeleteIcon />}
              onClick={() => deleteItem(id.toString())}
              label="Odstranit"
            />,
          ]
        },
        {
          field: 'vsTitle', headerName: 'Zdroj', flex: 1,
        },
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
  </Container>;
}

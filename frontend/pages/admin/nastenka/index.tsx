import React from "react";
import { Container } from "@mui/material";
import { NextLinkComposed } from "components/Link";
import { useAnnouncementListQuery, useDeleteAnnouncementMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import { useRouter } from 'next/router';
import { useSnackbar } from 'notistack';
import { useConfirm } from 'material-ui-confirm';
import format from "date-fns/format";

export default function AnnouncementAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const confirm = useConfirm();
  const { enqueueSnackbar } = useSnackbar();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useAnnouncementListQuery({
    limit, offset: page * limit,
  });

  const rowCount = data?.upozornenis?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  const { mutateAsync: doDeleteItem } = useDeleteAnnouncementMutation({
    onSuccess: () => refetch(),
  });
  const deleteItem = React.useCallback(async (id: string) => {
    await confirm({ description: 'Opravdu chcete smazat příspěvek?' });
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
    <NextLinkComposed href="/admin/nastenka/add" className="btn btn-primary">Přidat</NextLinkComposed>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      getRowId={row => row.upId}
      rows={data?.upozornenis?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/nastenka/edit/${id}`)}
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
          field: 'name', headerName: 'Jméno', flex: 1,
          renderCell: ({ row }) => <>
            <big>{row.upNadpis} </big>
            {row.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div>
              {row.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
                <div className="box"
                  key={g.skupinyByUpsIdSkupina?.sId}
                  title={g.skupinyByUpsIdSkupina?.sName}
                  style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
                />
              )}
            </div>}
          </>,
        },
        {
          field: 'author', headerName: 'Autor', flex: 1,
          renderCell: ({ row }) => <>
            {row.userByUpKdo?.uJmeno} {row.userByUpKdo?.uPrijmeni}{', '}
            {row.upTimestampAdd ? format(new Date(row.upTimestampAdd), 'd. M. y') : ''}
          </>,
        },
      ]}
    />
  </Container>;
}

import React from "react";
import { Container } from "@mui/material";
import { NextLinkComposed } from "components/Link";
import { useAnnouncementListQuery, useDeleteAnnouncementMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { useRouter } from 'next/router';
import format from "date-fns/format";
import { DeleteButton } from "components/DeleteButton";

export default function AnnouncementAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useAnnouncementListQuery({ limit, offset: page * limit });

  const rowCount = data?.upozornenis?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  const { mutateAsync: doDelete } = useDeleteAnnouncementMutation({
    onSuccess: () => refetch(),
  });

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
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/nastenka/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat příspěvek" />,
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

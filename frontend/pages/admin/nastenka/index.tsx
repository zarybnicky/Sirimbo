import React from "react";
import { useAnnouncementListQuery, useDeleteAnnouncementMutation } from "lib/graphql/Announcement";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { useRouter } from 'next/router';
import { format } from "date-fns";
import { DeleteButton } from "components/DeleteButton";
import { Button } from "components/Button";

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

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/nastenka/add">Přidat</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
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
                <div className="w-3.5 h-3.5"
                  key={g.skupinyByUpsIdSkupina?.id}
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
  </div>;
}

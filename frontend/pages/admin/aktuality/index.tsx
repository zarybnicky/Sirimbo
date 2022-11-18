import * as React from 'react';
import { useArticlesQuery, useDeleteArticleMutation } from 'lib/graphql/Articles';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { useRouter } from 'next/router';
import { DeleteButton } from 'components/DeleteButton';
import { Button } from 'components/Button';
import { formatFullDate } from 'lib/format-date';

export default function ArticleAdminList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useArticlesQuery({ limit, offset: page * limit });
  const { mutateAsync: doDelete } = useDeleteArticleMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.aktualities?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <div className="container mx-auto max-w-5xl mt-12 mb-8">
    <Button href="/admin/aktuality/add">Nový článek</Button>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      rows={data?.aktualities?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/aktuality/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat článek" />,
          ],
        },
        { field: 'atJmeno', headerName: 'Jméno', flex: 1 },
        {
          field: 'author', headerName: 'Autor', flex: 1,
          renderCell: ({ row }) => row.atTimestampAdd ? formatFullDate(new Date(row.atTimestampAdd)) : '',
        },
      ]}
    />
  </div>;
}

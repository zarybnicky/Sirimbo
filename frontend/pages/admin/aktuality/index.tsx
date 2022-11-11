import * as React from 'react';
import format from 'date-fns/format';
import { useArticlesQuery, useDeleteArticleMutation } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { useRouter } from 'next/router';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';
import { Button } from 'components/Button';

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

  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
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
      getRowId={row => row.atId}
      rows={data?.aktualities?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/aktuality/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat článek" />,
          ],
        },
        { field: 'atJmeno', headerName: 'Jméno', flex: 1 },
        {
          field: 'author', headerName: 'Autor', flex: 1,
          renderCell: ({ row }) => row.atTimestampAdd ? format(new Date(row.atTimestampAdd), 'd. M. y') : '',
        },
      ]}
    />
  </Container>;
}

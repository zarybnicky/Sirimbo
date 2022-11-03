import * as React from 'react';
import format from 'date-fns/format';
import { NextLinkComposed } from 'components/Link';
import { useArticlesQuery, useDeleteArticleMutation } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { useRouter } from 'next/router';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';

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
    <NextLinkComposed href="/admin/aktuality/add" className="btn btn-primary">Nový článek</NextLinkComposed>

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
          getActions: ({ row }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/aktuality/edit/${row.atId}`)}
              label="Upravit"
            />,
            <DeleteButton
              key="delete" title="smazat článek"
              params={{ id: row.atId }} onDelete={doDelete}
            />,
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

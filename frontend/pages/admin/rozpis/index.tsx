import * as React from 'react';
import format from 'date-fns/format';
import { Checkbox, Container } from '@mui/material';
import { NextLinkComposed } from 'components/Link';
import { useScheduleListQuery, useDeleteScheduleMutation, useToggleScheduleVisibleMutation } from 'lib/graphql';
import { useRouter } from 'next/router';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import { DeleteButton } from 'components/DeleteButton';

export default function RozpisAdminList() {
  const router = useRouter();
  const [limit] = React.useState(30);
  const [page, setPage] = React.useState(0);
  const { data, refetch } = useScheduleListQuery({ limit, offset: page * limit });
  const { mutateAsync: toggleVisible } = useToggleScheduleVisibleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doDelete } = useDeleteScheduleMutation({
    onSuccess: () => refetch(),
  });

  const rowCount = data?.rozpis?.totalCount || 0;
  const [rowCountState, setRowCountState] = React.useState(rowCount);
  React.useEffect(() => {
    setRowCountState((prev) => rowCount !== undefined ? rowCount : prev);
  }, [rowCount]);

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/rozpis/add" className="btn btn-primary">Nový rozpis</NextLinkComposed>

    <DataGrid
      page={page}
      onPageChange={setPage}
      pageSize={limit}
      rowsPerPageOptions={[limit]}
      rowCount={rowCountState}
      pagination
      paginationMode="server"
      autoHeight={true}
      getRowId={row => row.rId}
      rows={data?.rozpis?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions', flex: 1,
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/rozpis/edit/${id}`)}
              label="Upravit"
            />,
            <GridActionsCellItem key="edit-lessons"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/rozpis/detail/${id}`)}
              label="Upravit lekce"
            />,
            <GridActionsCellItem key="duplicate"
              icon={<ContentCopyIcon />}
              onClick={() => router.push(`/admin/rozpis/detail/${id}`)}
              label="Duplikovat"
            />,
            <DeleteButton
              key="delete" title="smazat rozpis"
              params={{ id: id.toString() }} onDelete={doDelete}
            />,
          ]
        },
        {
          field: 'vsTitle', headerName: 'Trenér', flex: 1,
          renderCell: ({ row }) => <>{row.userByRTrener?.uJmeno} {row.userByRTrener?.uPrijmeni}</>,
        },
        {
          field: 'date', headerName: 'Datum', flex: 1,
          renderCell: ({ row }) => row.rDatum ? format(new Date(row.rDatum), 'd. M. y') : '',
        },
        { field: 'rKde', headerName: 'Místo', flex: 1 },
        {
          field: 'visible', headerName: 'Viditelný', flex: 1,
          renderCell: ({ row }) => (
            <Checkbox checked={row.rVisible || false} onChange={() => toggleVisible({
              id: row.rId, visible: !row.rVisible,
            })} />
          ),
        },
      ]}
    />
  </Container>;
}

import * as React from 'react';
import { useGalleryDirQuery, useDeleteGalleryPhotoMutation } from 'lib/graphql/Gallery';
import { useRouter } from 'next/router';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';
import { GalleryDirForm } from "components/GalleryDirectoryForm";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function AdminGalleryPhotoList() {
  const router = useRouter();
  const { data, refetch } = useGalleryDirQuery({ id: router.query.id as string });
  const { mutateAsync: doDelete } = useDeleteGalleryPhotoMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    {data?.galerieDir && <GalleryDirForm data={data.galerieDir} onSuccess={() => router.back()} />}

    <DataGrid
      autoHeight={true}
      rows={data?.galerieDir?.galerieFotosByGfIdRodic.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/galerie/file/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={() => doDelete({ id: id as string })} title="smazat fotku" />,
          ]
        },
        { field: 'gfName', headerName: 'Jméno', flex: 1 },
      ]}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peGalerie, PermissionLevel.P_OWNED,
);

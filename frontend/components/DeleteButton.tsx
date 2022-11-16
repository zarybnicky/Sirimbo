import React from "react";
import { useConfirm } from 'components/Confirm';
import { Trash2 as DeleteIcon } from 'react-feather';
import { GridActionsCellItem } from '@mui/x-data-grid';
import { toast } from 'react-toastify';

export const DeleteButton = React.memo(function DeleteButton({
  title, id, onDelete
}: {
  title: string;
  id: string | number;
  onDelete: (params: { id: string; }) => Promise<unknown>;
}): React.ReactElement | null {
  const confirm = useConfirm();

  const deleteItem = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat ${title}?` });
    try {
      await onDelete({ id: id.toString() });
      toast.success('Smazáno');
    } catch (e) {
      if (e instanceof Error) {
        toast.error(e.message);
      } else {
        toast.error('Nepodařilo se smazat položku');
      }
    }
  }, [confirm, id, onDelete, title]);

  return <GridActionsCellItem key="delete" icon={<DeleteIcon />} onClick={deleteItem} label="Odstranit" />;
});

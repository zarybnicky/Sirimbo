import React from 'react';
import { useConfirm } from 'components/Confirm';
import { Trash2 as DeleteIcon } from 'react-feather';
import { toast } from 'react-toastify';

export const DeleteButton = React.memo(function DeleteButton({
  title,
  onDelete,
}: {
  title: string;
  onDelete: () => Promise<unknown>;
}): React.ReactElement | null {
  const confirm = useConfirm();

  const deleteItem = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat ${title}?` });
    try {
      await onDelete();
      toast.success('Smazáno');
    } catch (e) {
      if (e instanceof Error) {
        toast.error(e.message);
      } else {
        toast.error('Nepodařilo se smazat položku');
      }
    }
  }, [confirm, onDelete, title]);

  return (
    <button
      onClick={deleteItem}
      className="shadow-md hover:bg-stone-50 flex items-center gap-1 px-3 rounded-2xl py-1 text-xs tracking-tight font-bold"
    >
      <DeleteIcon className="w-4" /> Odstranit
    </button>
  );
});

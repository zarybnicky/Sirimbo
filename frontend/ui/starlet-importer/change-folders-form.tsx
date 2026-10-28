import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { fetchStarlet } from '@/starlet/query';
import { FoldersAndSeasonsDocument } from '@/starlet/graphql/Query';
import { VerticalCheckboxButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useEffect, useState } from 'react';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { isTruthy } from '@/lib/truthyFilter';

const Form = z.object({
  folders: z.array(z.string()),
  seasons: z.array(z.string()),
});

type FolderOrSeason = {
  key: string;
  name: string;
  order_value: number;
};

export function ChangeFoldersForm() {
  const { onSuccess } = useFormResult();
  const { folders: prevFolders, seasons: prevSeasons } =
    useAtomValue(starletSettingsAtom);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      folders: prevFolders.map(([id]) => id),
      seasons: prevSeasons.map(([id]) => id),
    },
  });
  const [result, updateSettings] = useMutation(UpdateTenantSettingsDocument);
  const token = useAtomValue(starletTokenAtom);

  const [folders, setFolders] = useState<FolderOrSeason[]>([]);
  const [seasons, setSeasons] = useState<FolderOrSeason[]>([]);

  useEffect(() => {
    if (!token?.auth_ok) return;

    fetchStarlet(FoldersAndSeasonsDocument, {}).then(({ folders, seasons }) => {
      if (!folders || !seasons) return;
      setFolders(
        (folders.filter(isTruthy) as FolderOrSeason[]).toSorted(
          (x, y) => x.order_value - y.order_value,
        ),
      );
      setSeasons(
        (seasons.filter(isTruthy) as FolderOrSeason[]).toSorted(
          (x, y) => x.order_value - y.order_value,
        ),
      );
    });
  }, [token?.auth_ok]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const selectedFolders = values.folders.map((id) => [
      id,
      folders.find((x) => x.key === id)?.name ?? '?',
    ]);
    const foldersResult = await updateSettings({
      input: {
        path: ['evidenceFolders'],
        newValue: selectedFolders,
      },
    });
    if (foldersResult.error) return;

    const selectedSeasons = values.seasons.map((id) => [
      id,
      seasons.find((x) => x.key === id)?.name ?? '?',
    ]);
    const seasonsResult = await updateSettings({
      input: {
        path: ['evidenceSeasons'],
        newValue: selectedSeasons,
      },
    });
    if (!seasonsResult.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <div className="grid grid-cols-2 gap-2">
        <VerticalCheckboxButtonGroupElement
          control={control}
          name="folders"
          options={folders.map((folder) => ({ id: folder.key, label: folder.name }))}
        />
        <VerticalCheckboxButtonGroupElement
          control={control}
          name="seasons"
          options={seasons.map((season) => ({ id: season.key, label: season.name }))}
        />
      </div>
      <SubmitButton control={control} />
    </form>
  );
}

import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { fetchStarlet } from '@/starlet/query';
import { FoldersAndSeasonsDocument } from '@/starlet/graphql/Query';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React, { useEffect, useState } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { isTruthy } from '@/lib/truthyFilter';

const Form = z.object({
  folders: z.record(z.string(), z.boolean().prefault(false)),
  seasons: z.record(z.string(), z.boolean().prefault(false)),
});

type FolderOrSeason = {
  key: string;
  name: string;
  order_value: number;
};

export function ChangeFoldersForm() {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useForm({
    resolver: zodResolver(Form),
  });
  const updateSettings = useMutation(UpdateTenantSettingsDocument)[1];
  const token = useAtomValue(starletTokenAtom);

  const { folders: prevFolders, seasons: prevSeasons } =
    useAtomValue(starletSettingsAtom);
  useEffect(() => {
    reset({
      folders: Object.fromEntries(prevFolders.map((x) => [x[0], true] as const)),
      seasons: Object.fromEntries(prevSeasons.map((x) => [x[0], true] as const)),
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

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

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await updateSettings({
      input: {
        path: ['evidenceFolders'],
        newValue: JSON.stringify(
          Object.entries(values.folders)
            .filter((x) => x[1])
            .map((x) => [x[0], folders.find((y) => y.key === x[0])?.name ?? '?']),
        ),
      },
    });
    await updateSettings({
      input: {
        path: ['evidenceSeasons'],
        newValue: JSON.stringify(
          Object.entries(values.seasons)
            .filter((x) => x[1])
            .map((x) => [x[0], seasons.find((y) => y.key === x[0])?.name ?? '?']),
        ),
      },
    });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <div className="grid grid-cols-2">
        <ul>
          {folders.map((x) => (
            <li key={x.key}>
              <CheckboxElement
                control={control}
                name={`folders.${x.key}`}
                label={x.name}
              />
            </li>
          ))}
        </ul>
        <ul>
          {seasons.map((x) => (
            <li key={x.key}>
              <CheckboxElement
                control={control}
                name={`seasons.${x.key}`}
                label={x.name}
              />
            </li>
          ))}
        </ul>
      </div>
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}

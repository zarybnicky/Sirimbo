import {
  EvidenceStarletDocument,
  UpdateTenantSettingsDocument,
} from '@/graphql/CurrentUser';
import { fetchGql } from '@/lib/query';
import { FoldersAndSeasonsDocument } from '@/starlet/graphql/Query';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { print } from '@0no-co/graphql.web';
import React, { useEffect, useState } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

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
  const update = useMutation(UpdateTenantSettingsDocument)[1];
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

    fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/graphql',
      data: JSON.stringify({
        query: print(FoldersAndSeasonsDocument),
        variables: {},
      }),
      auth: token.auth_token,
    }).then((x) => {
      const data = JSON.parse(x.evidenceStarlet).data;
      setFolders(
        data.folders.toSorted(
          (x: FolderOrSeason, y: FolderOrSeason) => x.order_value - y.order_value,
        ),
      );
      setSeasons(
        data.seasons.toSorted(
          (x: FolderOrSeason, y: FolderOrSeason) => x.order_value - y.order_value,
        ),
      );
    });
  }, [token]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await update({
      input: {
        path: ['evidenceFolders'],
        newValue: JSON.stringify(
          Object.entries(values.folders)
            .filter((x) => x[1])
            .map((x) => [x[0], folders.find((y) => y.key === x[0])?.name ?? '?']),
        ),
      },
    });
    await update({
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
        {folders && (
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
        )}
        {seasons && (
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
        )}
      </div>
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}

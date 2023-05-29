import {
  CohortGroupDocument,
  CohortGroupListDocument,
  CreateCohortGroupDocument,
  DeleteCohortGroupDocument,
  UpdateCohortGroupDocument,
} from 'lib/graphql/CohortGroup';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { useQueryClient } from '@tanstack/react-query';
import { toast } from 'react-toastify';
import dynamic from 'next/dynamic';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });
import { CohortListDocument, UpdateCohortDocument } from 'lib/graphql/Cohorts';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import { Plus, Trash2 } from 'lucide-react';
import { CollapsibleCard } from './CollapsibleCard';
import { Command, CommandItem, CommandInput, CommandList } from 'components/ui/command';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Item } from './layout/Item';
import { useRouter } from 'next/router';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { ErrorPage } from './ErrorPage';

const Form = z.object({
  name: z.string(),
  description: z.string().default(''),
  isPublic: z.boolean().default(false),
  ordering: z.number().default(0),
});
type FormProps = z.infer<typeof Form>;

type Props = {
  id?: string;
};

const backHref: Route = { pathname: '/admin/cohort-group' };

export function CohortGroupForm({ id = '' }: Props) {
  const router = useRouter();
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(getGqlKey(CohortGroupListDocument, {}));
  }, [queryClient]);
  const query = useGqlQuery(CohortGroupDocument, { id }, { enabled: !!id, cacheTime: 0 });
  const data = query.data?.cohortGroup;

  const create = useGqlMutation(CreateCohortGroupDocument, { onSuccess });
  const update = useGqlMutation(UpdateCohortGroupDocument, { onSuccess });

  const { data: cohorts } = useGqlQuery(CohortListDocument, {});
  const remaining = React.useMemo(() => {
    const used = (data?.skupiniesByCohortGroup?.nodes || []).map((x) => x.id);
    return (cohorts?.skupinies?.nodes || []).filter((x) => !used.includes(x.id));
  }, [cohorts, data]);

  const updateCohort = useGqlMutation(UpdateCohortDocument, { onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.optional().parse(data));
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update.mutateAsync({ id, patch });
    } else {
      const res = await create.mutateAsync({ input: patch });
      const id = res.createCohortGroup?.cohortGroup?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/cohort-group/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.cohortGroup === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form
      className="container flex flex-col gap-2"
      onSubmit={handleSubmit(onSubmit.execute)}
    >
      <Item.Titlebar
        backHref={backHref}
        title={data ? data.name || '(Bez názvu)' : 'Nový tréninkový program'}
      >
        {data && (
          <DeleteButton
            doc={DeleteCohortGroupDocument}
            id={id}
            title="smazat tréninkový program"
            onDelete={() => {
              router.push(backHref);
              queryClient.invalidateQueries(getGqlKey(CohortGroupListDocument, {}));
            }}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </Item.Titlebar>

      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <CheckboxElement control={control} name="isPublic" label="Zobrazit pro veřejnost" />
      <TextFieldElement
        control={control}
        type="number"
        name="ordering"
        label="Pořadí v seznamech skupin (1 = první, 999 = poslední)"
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Popis"
      />

      {data && (
        <div className="mt-1 pb-8">
          <div className="text-stone-700 text-sm pb-1">Tréninkové skupiny v programu</div>

          {data.skupiniesByCohortGroup.nodes.map((x) => (
            <CollapsibleCard key={x.id} title={x.sName} cohort={x}>
              <button
                className="button button-white gap-2"
                onClick={() =>
                  updateCohort.mutate({ id: x.id, patch: { cohortGroup: null } })
                }
              >
                <Trash2 /> Odstranit
              </button>
            </CollapsibleCard>
          ))}

          <CollapsibleCard
            title={
              <span>
                <Plus className="inline w-4 h-4 text-stone-600" /> Přidat skupinu
              </span>
            }
          >
            <Command className="border">
              <CommandInput placeholder="Vyhledat..." />
              <CommandList>
                {remaining.map((x) => (
                  <CommandItem
                    key={x.id}
                    value={x.sName}
                    onSelect={() =>
                      updateCohort.mutate({ id: x.id, patch: { cohortGroup: data.id } })
                    }
                  >
                    {x.sName}
                  </CommandItem>
                ))}
              </CommandList>
            </Command>
          </CollapsibleCard>
        </div>
      )}
    </form>
  );
}

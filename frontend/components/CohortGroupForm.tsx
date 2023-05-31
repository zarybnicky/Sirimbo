import {
  CohortGroupDocument,
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
import { toast } from 'react-toastify';
import dynamic from 'next/dynamic';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });
import { CohortListDocument, UpdateCohortDocument } from 'lib/graphql/Cohorts';
import { Plus } from 'lucide-react';
import { Command, CommandItem, CommandInput, CommandList } from 'components/ui/command';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Item } from './layout/Item';
import { useRouter } from 'next/router';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { ErrorPage } from './ErrorPage';
import { Card } from './Card';
import * as Popover from '@radix-ui/react-popover';
import { cn } from 'lib/utils';
import { useMutation, useQuery } from 'urql';

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
  const [{ data: cohorts }] = useQuery({query: CohortListDocument});
  const [query] = useQuery({query: CohortGroupDocument, variables: { id }, pause: !id });
  const data = query.data?.cohortGroup;

  const create = useMutation(CreateCohortGroupDocument)[1];
  const update = useMutation(UpdateCohortGroupDocument)[1];
  const updateCohort = useMutation(UpdateCohortDocument)[1];

  const remaining = React.useMemo(() => {
    const used = (data?.skupiniesByCohortGroup?.nodes || []).map((x) => x.id);
    return (cohorts?.skupinies?.nodes || []).filter((x) => !used.includes(x.id));
  }, [cohorts, data]);

  const { reset, control, handleSubmit } = useForm<FormProps>({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.optional().parse(data));
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data!.createCohortGroup?.cohortGroup?.id;
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
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <Item.Titlebar
        backHref={backHref}
        title={data ? data.name || '(Bez názvu)' : 'Nový tréninkový program'}
      >
        {data && (
          <DeleteButton
            doc={DeleteCohortGroupDocument}
            id={id}
            title="smazat tréninkový program"
            onDelete={() => router.push(backHref)}
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

      {id && (
        <div className="mt-1 pb-8">
          <div className="text-stone-700 text-sm pb-1">Tréninkové skupiny v programu</div>

          {data?.skupiniesByCohortGroup.nodes.map((x) => (
            <Card
              key={x.id}
              cohort={x}
              menu={[
                {
                  title: 'Odebrat',
                  onClick: () => updateCohort({ id: x.id, patch: { cohortGroup: null } }),
                },
              ]}
            >
              {x.sName}
            </Card>
          ))}

          <Popover.Root>
            <Popover.Trigger asChild>
              <button className="button button-white">
                  <Plus className="inline w-4 h-4" /> Přidat skupinu
              </button>
            </Popover.Trigger>
            <Popover.Content
              align="start"
              sideOffset={4}
              className={cn(
                'z-20 radix-side-top:animate-slide-up radix-side-bottom:animate-slide-down',
                'w-48 rounded-lg shadow-md md:w-56',
                'bg-white dark:bg-stone-800',
              )}
            >
              <Popover.Arrow className="fill-current text-white dark:text-stone-800" />
              <Command className="border">
                <CommandInput autoFocus placeholder="Vyhledat..." />
                <CommandList>
                  {remaining.map((x) => (
                    <CommandItem
                      key={x.id}
                      value={x.sName}
                      onSelect={() => updateCohort({ id: x.id, patch: { cohortGroup: id } })}
                    >
                      {x.sName}
                    </CommandItem>
                  ))}
                </CommandList>
              </Command>
            </Popover.Content>
          </Popover.Root>
        </div>
      )}
    </form>
  );
}

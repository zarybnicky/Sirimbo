import { TenantForm } from '@app/ui/TenantForm';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import React from 'react';
import { useAuth } from '@app/ui/use-auth';

const Page = () => {
  const [mode, setMode] = React.useState<'view' | 'edit'>('view');
  const { tenants } = useAuth();

  return (
    <Layout requireAdmin>
      <NextSeo title="Organizace" />
      {mode === 'edit' ? (
        <TenantForm />
      ) : (
        <>
          {JSON.stringify(tenants)}
        </>
      )}
    </Layout>
  );
};

export default Page;

/* const LessonForm = z.object({
*   riOd: z.string().regex(/[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?/),
*   riDo: z.string().regex(/[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?/),
*   riPartner: z.string().nullish(),
*   riLock: z.boolean().optional(),
* }).refine((form) => form.riOd < form.riDo, 'Čas začátku musí být před časem konce')
* type LessonFormProps = z.infer<typeof LessonForm>;
*
* function LessonAdminForm({ lesson }: { lesson: ScheduleItemBasicFragment }) {
*   const [{ data: couples }] = useQuery({ query: CoupleListDocument });
*   const [mode, setMode] = React.useState<'view' | 'edit'>('view');
*   const { reset, control } = useForm<LessonFormProps>({
*     resolver: zodResolver(LessonForm),
*   });
*   const couple = lesson.paryByRiPartner;
*
*   React.useEffect(() => {
*     reset(LessonForm.innerType().partial().optional().parse(lesson));
*   }, [mode, reset, lesson]);
*
*   return mode === 'view' ? (
*     <Card
*       className="flex gap-2 tabular-nums cursor-pointer"
*       onClick={() => setMode('edit')}
*     >
*       <div>
*         {lesson.riOd.substring(0, 5)} - {lesson.riDo.substring(0, 5)}
*       </div>
*       <div className="grow">{couple ? formatCoupleName(couple) : 'VOLNÁ'}</div>
*     </Card>
*   ) : (
*     <Card>
*         <button
*           type="button"
*           onClick={() => setMode('view')}
*         >
*           <X />
*         </button>
*
*         <TextFieldElement control={control} name="riOd" type="text" />
*         <TextFieldElement control={control} name="riDo" type="text" />
*         <ComboboxElement
*           control={control}
*           name="riPartner"
*           placeholder="vyberte pár"
*           options={(couples?.activeCouples?.nodes || []).map(x => ({ id: x.id, label: formatCoupleName(x) }))}
*         />
*         <CheckboxElement control={control} name="riLock" />
*     </Card>
*   );
* } */

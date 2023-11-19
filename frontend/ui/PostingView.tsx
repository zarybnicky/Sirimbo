import { PostingFragment } from "@/graphql/Payment";
import { formatDefaultEventName, formatEventType, moneyFormatter, numericDateFormatter } from "./format";

export function PostingView({ posting: x }: { posting: PostingFragment }) {
  let date = x?.transaction?.payment?.paidAt;
  let description = x.transaction?.description;

  let event = x.transaction?.payment?.eventInstance?.event
  if (event) {
    description = parseFloat(x.amount) < 0 ? ((formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')) : formatDefaultEventName(event);
    date = x.transaction?.payment?.eventInstance?.since
  }

  event = x.transaction?.payment?.eventRegistration?.event;
  if (event) {
    description = formatDefaultEventName(event);
    date = event.eventInstancesList?.[0]?.since;
  }

  const cohort = x.transaction?.payment?.cohortSubscription?.cohort
  if (cohort) {
    description = `Příspěvky: ${cohort.sName}`;
  }

  return (
    <div key={x.id} className="justify-between gap-2 flex flex-wrap">
      <span>
        {date ? numericDateFormatter.format(new Date(date)) : ''}{' '}
        {description}
      </span>
      <span>{moneyFormatter.format(parseFloat(x.amount))}</span>
    </div>
  );
}

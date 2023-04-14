import application from './application'
import countingChange from './counting-change'
import factorial from './factorial'
import hanoi from './hanoi'
import lazyGenerator from './lazy-generator'
import nQueues from './n-queues'
import oddOrEven from './odd-or-even'
import callcc from './return-with-callcc'
import yinYang from './yin-yang'

const programs: Record<string, string> = {
  factorial,
  countingChange,
  application,
  hanoi,
  lazyGenerator,
  nQueues,
  oddOrEven,
  callcc,
  yinYang
}

export default programs

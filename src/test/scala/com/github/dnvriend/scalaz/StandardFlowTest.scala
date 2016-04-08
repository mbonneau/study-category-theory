/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend.scalaz

import com.github.dnvriend.TestSpec

class StandardFlowTest extends TestSpec {
  /**
   * 1. Process single cmd elements eg: Source(() => request.iterator)
   * 2. Get the domain objects from persistency .mapAsync(1)(getDomainObjects)
   * 3. Validate using business rules .map(validate)
   * 4. Determine instructions to be executed .map(determineActions)
   * 5. Execute the actions .mapAsync(1)(executeActions)
   * 6. Collect the results and return .runFold(List.empty[ReturnType])(_ :+ _)
   */

}

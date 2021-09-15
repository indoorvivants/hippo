package example.frontend.components

import com.raquo.laminar.api.L.*

case class SearchBox private (node: Element, signal: Signal[String])

object SearchBox:
  def create =
    val node = input(
      `type` := "text",
      idAttr := "search-filter"
    )

    val stream =
      node.events(onInput).mapTo(node.ref.value).startWith("")

    new SearchBox(node, stream)

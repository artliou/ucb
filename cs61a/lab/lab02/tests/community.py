test = {
  'name': 'Community',
  'points': 0,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> def troy():
          ...     abed = 0
          ...     while abed < 3:
          ...         britta = lambda: abed
          ...         print(abed)
          ...         abed += 2
          ...     annie = abed
          ...     annie += 1
          ...     abed = 6 # seasons and a movie
          ...     return britta
          >>> jeff = troy()
          0
          2
          >>> shirley = lambda: jeff
          >>> pierce = shirley()
          >>> pierce()
          13e9492c801fddcd5c1330b411f26ac8
          # locked
          """,
          'hidden': False,
          'locked': True
        }
      ],
      'scored': False,
      'type': 'wwpp'
    }
  ]
}